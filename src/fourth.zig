const std = @import("std");
const unicode = std.unicode;
const Allocator = std.mem.Allocator;
const testing = std.testing;
const assert = std.debug.assert;

pub const Repl = struct {
    gpa: Allocator,
    words: std.StringArrayHashMapUnmanaged(WordId) = .{},

    primitives: std.AutoArrayHashMapUnmanaged(WordId, *const PrimitiveFn) = .{},
    contexts: std.AutoArrayHashMapUnmanaged(WordId, *anyopaque) = .{},
    signatures: std.AutoArrayHashMapUnmanaged(WordId, []const u8) = .{},
    descriptions: std.AutoArrayHashMapUnmanaged(WordId, []const u8) = .{},

    glossary: std.AutoArrayHashMapUnmanaged(WordId, Definition) = .{},
    stack: std.ArrayListUnmanaged(Integer) = .{},
    return_stack: std.ArrayListUnmanaged(Integer) = .{},
    count: u32,

    state: State,

    const State = union(enum) {
        execute,
        definition_start,
        definition_body: struct {
            word: []const u8,
            buffer: std.ArrayListUnmanaged(Entity),
        },

        pub fn format(
            state: State,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;

            switch (state) {
                .execute => try writer.writeAll("EXECUTE"),
                .definition_start => try writer.writeAll("DEFINITION_START"),
                .definition_body => |body| try writer.print("DEFINITION_BODY(word={s} buffer_len={})", .{ body.word, body.buffer.items.len }),
            }
        }
    };

    pub const Error = error{
        StackUnderflow,
        OutOfMemory,
        DivisionByZero,
        InvalidInput,
        SystemError,
    } || std.fs.File.WriteError;

    pub const PrimitiveFn = fn (*Repl, ?*anyopaque) Repl.Error!void;
    pub const PrimitiveOptions = struct {
        name: []const u8,
        signature: ?[]const u8 = null,
        description: ?[]const u8 = null,
        func: *const PrimitiveFn,
        ctx: ?*anyopaque = null,
    };

    pub const Integer = i32;
    const Definition = []const Entity;
    const Entity = struct {
        payload: union {
            integer: Integer,
            word: WordId,
        },
        tag: enum(u1) {
            integer,
            word,
        },

        pub fn format(
            entity: Entity,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;

            switch (entity.tag) {
                .word => try writer.print("word: {}", .{entity.payload.word}),
                .integer => try writer.print("integer: {}", .{entity.payload.integer}),
            }
        }
    };

    // TODO: pre define this function:
    // : .S CR 'S SO @ 2- DO I @ • -2 +LOOP ;~

    /// members of this enum are keywords for the forth language. They need to
    /// change state to be in this list.
    pub const WordId = enum(u32) {
        @":",
        @";",
        IF,
        THEN,
        ELSE,
        ABORT,
        FORGET,
        DEBUG,
        _,
    };

    pub fn init(allocator: Allocator) !Repl {
        var ret = Repl{
            .gpa = allocator,
            .count = 0,
            .state = .{ .execute = {} },
        };
        errdefer ret.deinit();

        for (@import("primitives.zig").defaults) |prim|
            try ret.add_primitive(prim);

        return ret;
    }

    pub fn deinit(repl: *Repl) void {
        for (repl.words.keys()) |key|
            repl.gpa.free(key);

        for (repl.signatures.values()) |sig|
            repl.gpa.free(sig);

        for (repl.descriptions.values()) |desc|
            repl.gpa.free(desc);

        for (repl.glossary.values()) |definition|
            repl.gpa.free(definition);

        repl.words.deinit(repl.gpa);

        repl.primitives.deinit(repl.gpa);
        repl.contexts.deinit(repl.gpa);
        repl.signatures.deinit(repl.gpa);
        repl.descriptions.deinit(repl.gpa);

        repl.glossary.deinit(repl.gpa);
        repl.stack.deinit(repl.gpa);
        repl.return_stack.deinit(repl.gpa);
    }

    fn dump_state(repl: *Repl) !void {
        const stderr = std.io.getStdErr().writer();
        try stderr.print("state={}\n", .{repl.state});
    }

    fn create_word_id(repl: *Repl) WordId {
        const word_id: WordId = @enumFromInt(@typeInfo(WordId).Enum.fields.len + repl.count);
        repl.count += 1;
        return word_id;
    }

    fn get_word_id(repl: *Repl, token: []const u8) ?WordId {
        return std.meta.stringToEnum(WordId, token) orelse repl.words.get(token);
    }

    fn execute_word(repl: *Repl, word_id: WordId) !void {
        var execute_stack = std.ArrayList(Entity).init(repl.gpa);
        defer execute_stack.deinit();

        try execute_stack.append(.{
            .tag = .word,
            .payload = .{
                .word = word_id,
            },
        });

        while (execute_stack.popOrNull()) |entity| switch (entity.tag) {
            .integer => try repl.stack.append(repl.gpa, entity.payload.integer),
            .word => if (repl.primitives.get(entity.payload.word)) |func| {
                try func(repl, repl.contexts.get(entity.payload.word));
            } else {
                const definition = repl.glossary.get(entity.payload.word).?;
                var it = std.mem.reverseIterator(definition);
                while (it.next()) |e| {
                    try execute_stack.append(e);
                }
            },
        };
    }

    fn feed_token(repl: *Repl, token: []const u8) !void {
        switch (repl.state) {
            .execute => {
                if (std.meta.stringToEnum(WordId, token)) |keyword| switch (keyword) {
                    .@":" => {
                        repl.state = .{ .definition_start = {} };
                        return;
                    },
                    .@";" => return error.NotDefiningFunction,
                    .IF => return error.TODO,
                    .THEN => return error.TODO,
                    .ELSE => return error.TODO,
                    .ABORT => {},
                    .FORGET => return error.TODO,
                    .DEBUG => return try repl.dump_state(),
                    _ => {},
                };

                if (repl.get_word_id(token)) |word_id| {
                    return if (repl.primitives.get(word_id)) |prim|
                        prim(repl, repl.contexts.get(word_id))
                    else
                        repl.execute_word(word_id);
                }

                try repl.stack.append(repl.gpa, std.fmt.parseInt(Integer, token, 0) catch {
                    // TODO: print error
                    return error.UnknownSymbol;
                });
            },
            .definition_start => {
                if (std.meta.stringToEnum(WordId, token)) |keyword| switch (keyword) {
                    .@":" => return error.AlreadyStartedDefinition,
                    // treat as an exit out of defining a word
                    .ABORT, .@";" => {
                        repl.state = .{ .execute = {} };
                        return;
                    },
                    .IF => return error.TODO,
                    .THEN => return error.TODO,
                    .ELSE => return error.TODO,
                    .FORGET => return error.TODO,
                    .DEBUG => return try repl.dump_state(),
                    _ => {},
                };

                // TODO: name cannot be a number
                //if (std.fmt.parseInt(i32, token, 0)) {
                //    return error.WordIsANumber;
                //} else {}

                repl.state = .{
                    .definition_body = .{
                        .word = try repl.gpa.dupe(u8, token),
                        .buffer = std.ArrayListUnmanaged(Entity){},
                    },
                };
            },
            .definition_body => |*state| {
                errdefer {
                    repl.gpa.free(state.word);
                    repl.state = .{ .execute = {} };
                }

                if (std.meta.stringToEnum(WordId, token)) |keyword| switch (keyword) {
                    .@":" => return error.AlreadyStartedDefinition,
                    // treat as an exit out of defining a word
                    .ABORT => {
                        repl.state = .{ .execute = {} };
                        return;
                    },
                    .@";" => {
                        const word_id = repl.create_word_id();

                        try repl.words.putNoClobber(repl.gpa, state.word, word_id);

                        var owned = try state.buffer.toOwnedSlice(repl.gpa);
                        errdefer repl.gpa.free(owned);

                        try repl.glossary.putNoClobber(repl.gpa, word_id, owned);

                        repl.state = .{ .execute = {} };
                        return;
                    },
                    .IF => return error.TODO,
                    .THEN => return error.TODO,
                    .ELSE => return error.TODO,
                    .FORGET => return error.TODO,
                    .DEBUG => return try repl.dump_state(),
                    _ => {},
                };

                try repl.state.definition_body.buffer.append(repl.gpa, entity: {
                    const integer = std.fmt.parseInt(Integer, token, 0) catch {
                        const word_id = repl.get_word_id(token) orelse return error.SymbolNotFound;
                        break :entity Entity{
                            .tag = .word,
                            .payload = .{
                                .word = word_id,
                            },
                        };
                    };

                    break :entity Entity{
                        .tag = .integer,
                        .payload = .{
                            .integer = integer,
                        },
                    };
                });
            },
        }
    }

    pub fn feed_line(repl: *Repl, line: []const u8) !void {
        var it = std.mem.tokenizeScalar(u8, line, ' ');
        while (it.next()) |token| try repl.feed_token(token);
    }

    pub fn pop(repl: *Repl) Error!Integer {
        return repl.stack.popOrNull() orelse error.StackUnderflow;
    }

    pub fn push(repl: *Repl, value: Integer) Error!void {
        try repl.stack.append(repl.gpa, value);
    }

    pub fn pop_return(repl: *Repl) Error!Integer {
        return repl.return_stack.popOrNull() orelse error.StackUnderflow;
    }

    pub fn push_return(repl: *Repl, value: Integer) Error!void {
        try repl.return_stack.append(repl.gpa, value);
    }

    pub fn add_primitive(repl: *Repl, opts: PrimitiveOptions) !void {
        if (std.meta.stringToEnum(WordId, opts.name) != null)
            return error.CantOverwriteKeyword;

        if (repl.words.contains(opts.name))
            return error.WordExists;

        const word_id = repl.create_word_id();
        const name = try repl.gpa.dupe(u8, opts.name);
        errdefer repl.gpa.free(name);

        try repl.words.putNoClobber(repl.gpa, name, word_id);
        errdefer _ = repl.words.swapRemove(name);

        try repl.primitives.putNoClobber(repl.gpa, word_id, opts.func);
        errdefer _ = repl.primitives.swapRemove(word_id);

        if (opts.signature) |sig| {
            const sig_copy = try repl.gpa.dupe(u8, sig);
            errdefer repl.gpa.free(sig_copy);

            try repl.signatures.putNoClobber(repl.gpa, word_id, sig_copy);
        }
        errdefer if (opts.signature != null) {
            const entry = repl.signatures.fetchSwapRemove(word_id).?;
            repl.gpa.free(entry.value);
        };

        if (opts.description) |desc| {
            const desc_copy = try repl.gpa.dupe(u8, desc);
            errdefer repl.gpa.free(desc_copy);

            try repl.descriptions.putNoClobber(repl.gpa, word_id, desc_copy);
        }
        errdefer if (opts.description != null) {
            const entry = repl.descriptions.fetchSwapRemove(word_id).?;
            repl.gpa.free(entry.value);
        };
    }
};

fn is_boolean(value: Repl.Integer) bool {
    return switch (value) {
        0, -1 => true,
        else => false,
    };
}

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectError = std.testing.expectError;

test {
    std.testing.refAllDecls(@import("primitives.zig"));
}

test "feed unknown symbol" {
    var repl = try Repl.init(std.testing.allocator);
    defer repl.deinit();

    try expectError(error.UnknownSymbol, repl.feed_line("what"));
}

test "create definition and use it" {
    var repl = try Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line(": foo  1 + ;");

    try expect(repl.words.contains("foo"));
    try expectEqual(@as(usize, 1), repl.words.count());
    try expectEqual(@as(usize, 1), repl.glossary.count());

    try repl.feed_line("3 foo");
    try expectEqual(@as(usize, 1), repl.stack.items.len);
    try expectEqual(@as(i32, 4), repl.stack.items[0]);
}

test "create multi lined definition and use it" {
    var repl = try Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line(": foo");
    try repl.feed_line("    1 +");
    try repl.feed_line(";");

    try expect(repl.words.contains("foo"));

    try repl.feed_line("3 foo");
    try expectEqual(@as(usize, 1), repl.stack.items.len);
    try expectEqual(@as(i32, 4), repl.stack.items[0]);
}

test "feed integer" {
    var repl = try Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line("28");
    try expectEqual(@as(usize, 1), repl.stack.items.len);
    try expectEqual(@as(i32, 28), repl.stack.items[0]);
}

//==============================================================================
// Conditionals
//==============================================================================

test "inline if" {
    var repl = try Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line("1 2 + 3 = IF 5 THEN");
    try expectEqual(@as(usize, 1), repl.stack.items.len);
    try expectEqual(@as(i32, 5), repl.stack.items[0]);
}

test "inline if else" {
    var repl = try Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line("2 3 = IF 5 THEN 6 ELSE");
    try expectEqual(@as(usize, 1), repl.stack.items.len);
    try expectEqual(@as(i32, 6), repl.stack.items[0]);
}

test "nested inline if" {
    return error.TODO;
}

test "nested inline if else" {
    return error.TODO;
}

test "definition with if and use it" {
    var repl = try Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line(": ?FULL  12 = IF 42 THEN ;");
    try repl.feed_line("2 ?FULL");
    try expectEqual(@as(usize, 0), repl.stack.items.len);

    try repl.feed_line("12 ?FULL");
    try expectEqual(@as(usize, 1), repl.stack.items.len);
    try expectEqual(@as(i32, 42), repl.stack.items[0]);
}

test "definition with if else and use it" {
    var repl = try Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line(": ?FULL  12 = IF 42 THEN 50 ELSE ;");
    try repl.feed_line("2 ?FULL");
    try expectEqual(@as(usize, 1), repl.stack.items.len);
    try expectEqual(@as(i32, 50), repl.stack.items[0]);

    try repl.feed_line("12 ?FULL");
    try expectEqual(@as(usize, 1), repl.stack.items.len);
    try expectEqual(@as(i32, 42), repl.stack.items[0]);
}

test "definition with nested if" {
    return error.TODO;
}

test "definition with nested if else" {
    var repl = try Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line(": EGGSIZE ( n -- )           ");
    try repl.feed_line("   DUP 18 < IF  1   ELSE     ");
    try repl.feed_line("   DUP 21 < IF  2   ELSE     ");
    try repl.feed_line("   DUP 24 < IF  3   ELSE     ");
    try repl.feed_line("   DUP 27 < IF  4   ELSE     ");
    try repl.feed_line("   DUP 30 < IF  5   ELSE     ");
    try repl.feed_line("                42           ");
    try repl.feed_line("   THEN THEN THEN THEN THEN ;");

    try repl.feed_line("10 EGGSIZE");
    try repl.feed_line("19 EGGSIZE");
    try repl.feed_line("23 EGGSIZE");
    try repl.feed_line("25 EGGSIZE");
    try repl.feed_line("29 EGGSIZE");
    try repl.feed_line("50 EGGSIZE");

    try expectEqual(@as(usize, 6), repl.stack.items.len);
    try expectEqual(@as(i32, 1), repl.stack.items[0]);
    try expectEqual(@as(i32, 2), repl.stack.items[1]);
    try expectEqual(@as(i32, 3), repl.stack.items[2]);
    try expectEqual(@as(i32, 4), repl.stack.items[3]);
    try expectEqual(@as(i32, 5), repl.stack.items[4]);
    try expectEqual(@as(i32, 42), repl.stack.items[5]);
}

//==============================================================================
// Loops
//==============================================================================

//==============================================================================
// Variables
//==============================================================================

//==============================================================================
// Constants
//==============================================================================

//==============================================================================
// IO
//==============================================================================

//==============================================================================
// Compiler Extensions
//==============================================================================

// adding builtins

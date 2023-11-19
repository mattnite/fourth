const std = @import("std");
const unicode = std.unicode;
const Allocator = std.mem.Allocator;
const testing = std.testing;
const assert = std.debug.assert;

// TODO:
//
// need to consider how users are going to add builtins/native functions, and
// what those look like besides the normal builtins

pub const BuiltinFn = fn (*Repl) Repl.Error!void;

pub const Repl = struct {
    gpa: Allocator,
    symbols: std.StringArrayHashMap(WordId),
    // TODO: keep old definitions
    glossary: std.AutoArrayHashMap(WordId, Definition),
    stack: std.ArrayList(Integer),
    count: u32,

    state: union(enum) {
        normal,
        definition_start,
        definition_body: struct {
            word: []const u8,
            buffer: std.ArrayListUnmanaged(Entity),
        },
    },

    pub const Error = error{ StackUnderflow, OutOfMemory, DivisionByZero } || std.fs.File.WriteError;

    const Integer = i32;
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
    };

    // TODO: pre define this function:
    // : .S CR 'S SO @ 2- DO I @ • -2 +LOOP ;~

    const WordId = enum(u32) {
        // arithmetic
        @"*",
        @"+",
        @"-",
        @".",
        @"/",
        //@"/MOD",
        //MOD,
        //@"1+",
        //@"1-",
        //@"2+",
        //@"2-",
        //@"2*",
        //@"2/",
        //ABS,
        //NEGATE,
        //MIN,
        //MAX,
        //@"*/",
        //@"*/MOD",
        //U.
        //U*
        //U/MOD
        //U<
        //

        //// stack manipulation
        DROP,
        //@"2DROP",
        DUP,
        //@"2DUP",
        //@"?DUP",
        OVER,
        //@"2OVER",
        ROT,
        SWAP,
        //@"2SWAP",

        // the return stack
        //@">R",
        //@"R>",
        //I,
        //@"I'",
        //J,

        // definitions
        //FORGET,
        @":",
        //EMPTY,

        // comments
        //@"(",
        //@")",

        //// control flow
        //IF,
        //THEN,
        //NOT,
        //ELSE,
        //DO,
        //LOOP,
        //@"+LOOP",
        ///LOOP
        //BEGIN
        //UNTIL
        //QUIT
        //LEAVE
        //PAGE
        //QUIT
        //WHILE
        //REPEAT
        //U.R

        //// logical operators
        //AND,
        //OR,

        //// comparison
        //@"=",
        //@"<",
        //@">",
        //@"0=",
        //@"0<",
        //@"0>",

        //// misc editor commands
        //@"^",

        // editor
        //LIST,
        //LOAD,
        //FLUSH,
        //COPY,
        //WIPE,

        // line editing
        //T,
        //P,
        //U,
        //M,
        //X,

        // character editing
        //F,
        //S,
        //E,
        //D,
        //TILL,
        //I,
        //R,

        //@"ABORT\"",
        //@"?STACK",
        //EMIT
        //BS
        //LF
        //CR
        //NUMBER
        //INTERPRET
        //HEX
        //OCTAL
        //DECIMAL
        //D.

        // number formatting
        //#
        //<#
        //#S
        //c HOLD
        //SIGN
        //#>

        // double-length operators
        //

        // if it's not a normal
        _,
    };

    pub fn init(allocator: Allocator) Repl {
        return Repl{
            .gpa = allocator,
            .symbols = std.StringArrayHashMap(WordId).init(allocator),
            .glossary = std.AutoArrayHashMap(WordId, Definition).init(allocator),
            .stack = std.ArrayList(Integer).init(allocator),
            .count = 0,
            .state = .{ .normal = {} },
        };
    }

    pub fn deinit(repl: *Repl) void {
        for (repl.glossary.values()) |definition|
            repl.gpa.free(definition);

        for (repl.symbols.keys()) |key|
            repl.symbols.allocator.free(key);

        repl.glossary.deinit();
        repl.symbols.deinit();
        repl.stack.deinit();
    }

    fn get_word_id(repl: *Repl, token: []const u8) ?WordId {
        return std.meta.stringToEnum(WordId, token) orelse repl.symbols.get(token);
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
            .integer => try repl.stack.append(entity.payload.integer),
            .word => blk: {
                inline for (@typeInfo(WordId).Enum.fields) |field| {
                    if (@field(WordId, field.name) == entity.payload.word) {
                        const builtin_fn = @field(builtins, field.name);
                        try builtin_fn(repl);
                        break :blk;
                    }
                }

                const definition = repl.glossary.get(word_id).?;
                var it = std.mem.reverseIterator(definition);
                while (it.next()) |e|
                    try execute_stack.append(e);
            },
        };
    }

    fn feed_token(repl: *Repl, token: []const u8) !void {
        switch (repl.state) {
            .normal => {
                if (repl.get_word_id(token)) |word_id|
                    return repl.execute_word(word_id);

                try repl.stack.append(std.fmt.parseInt(Integer, token, 0) catch {
                    // TODO: print error
                    return error.UnknownSymbol;
                });
            },
            .definition_start => {
                if (std.mem.eql(u8, ":", token))
                    return error.AlreadyStartedDefinition;

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
                    repl.state = .{ .normal = {} };
                }

                if (token.len == 1) switch (token[0]) {
                    else => {},
                    ':' => return error.AlreadyStartedDefinition,
                    ';' => {
                        const word_id: WordId = @enumFromInt(@typeInfo(WordId).Enum.fields.len + repl.count);
                        repl.count += 1;

                        std.log.info("created word id for '{s}': {}", .{ state.word, word_id });
                        try repl.symbols.putNoClobber(state.word, word_id);

                        var owned = try state.buffer.toOwnedSlice(repl.gpa);
                        errdefer repl.gpa.free(owned);

                        try repl.glossary.putNoClobber(word_id, owned);

                        repl.state = .{ .normal = {} };
                        return;
                    },
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

    /// takes ownership of an allocated Definition
    pub fn create_definition(repl: *Repl, word: []const u8, definition: Definition) !WordId {
        errdefer definition.deinit(repl.allocator);

        if (repl.symbols.contains(word))
            return error.DefinitionExists;

        const word_id: WordId = @typeInfo(WordId).Enum.fields.len + repl.count;
        repl.count += 1;

        std.log.info("created word id: {}", .{word_id});
        const word_copy = try repl.gpa.dupe(word);
        errdefer repl.gpa.free(word_copy);

        try repl.symbols.putNoClobber(word_copy, word_id);
        try repl.glossary.putNoClobber(word_id, definition);
    }

    fn pop(repl: *Repl) Error!Integer {
        return repl.stack.popOrNull() orelse error.StackUnderflow;
    }
};

const builtins = struct {
    fn @":"(repl: *Repl) Repl.Error!void {
        assert(repl.state == .normal);
        repl.state = .{ .definition_start = {} };
    }

    fn DUP(repl: *Repl) Repl.Error!void {
        if (repl.stack.items.len == 0)
            return error.StackUnderflow;

        try repl.stack.append(repl.stack.items[repl.stack.items.len - 1]);
    }

    fn DROP(repl: *Repl) Repl.Error!void {
        _ = try repl.pop();
    }

    fn SWAP(repl: *Repl) Repl.Error!void {
        if (repl.stack.items.len < 2)
            return error.StackUnderflow;

        const len = repl.stack.items.len;
        std.mem.swap(Repl.Integer, &repl.stack.items[len - 1], &repl.stack.items[len - 2]);
    }

    fn @"."(repl: *Repl) Repl.Error!void {
        const stdio = std.io.getStdOut().writer();
        try stdio.print("{}\n", .{try repl.pop()});
    }

    fn @"+"(repl: *Repl) Repl.Error!void {
        const lhs = try repl.pop();
        const rhs = try repl.pop();
        try repl.stack.append(lhs + rhs);
    }

    fn @"-"(repl: *Repl) Repl.Error!void {
        const rhs = try repl.pop();
        const lhs = try repl.pop();
        try repl.stack.append(lhs - rhs);
    }

    fn @"*"(repl: *Repl) Repl.Error!void {
        const rhs = try repl.pop();
        const lhs = try repl.pop();
        try repl.stack.append(lhs * rhs);
    }

    fn @"/"(repl: *Repl) Repl.Error!void {
        const denominator = try repl.pop();
        const numerator = try repl.pop();

        if (denominator == 0)
            return error.DivisionByZero;

        try repl.stack.append(@divFloor(numerator, denominator));
    }

    fn OVER(repl: *Repl) Repl.Error!void {
        if (repl.stack.items.len < 2)
            return error.StackUnderflow;

        try repl.stack.append(repl.stack.items[repl.stack.items.len - 2]);
    }

    fn ROT(repl: *Repl) Repl.Error!void {
        if (repl.stack.items.len < 3)
            return error.StackUnderflow;

        const popped = repl.stack.orderedRemove(repl.stack.items.len - 2);
        try repl.stack.append(popped);
    }
};

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectError = std.testing.expectError;

test "feed unknown symbol" {
    var repl = Repl.init(std.testing.allocator);
    defer repl.deinit();

    try expectError(error.UnknownSymbol, repl.feed_line("what"));
}

test "create definiti(u1)on and use it" {
    var repl = Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line(": foo  1 + ;");

    try expect(repl.symbols.contains("foo"));
    try expectEqual(@as(usize, 1), repl.symbols.count());
    try expectEqual(@as(usize, 1), repl.glossary.count());

    try repl.feed_line("3 foo");
    try expectEqual(@as(usize, 1), repl.stack.items.len);
    try expectEqual(@as(i32, 4), repl.stack.items[0]);
}

test "feed integer" {
    var repl = Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line("28");
    try expectEqual(@as(usize, 1), repl.stack.items.len);
    try expectEqual(@as(i32, 28), repl.stack.items[0]);
}

test "DUP" {
    var repl = Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line("28 DUP");
    try expectEqual(@as(usize, 2), repl.stack.items.len);
    try expectEqual(@as(i32, 28), repl.stack.items[0]);
    try expectEqual(@as(i32, 28), repl.stack.items[1]);
}

test "DROP" {
    var repl = Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line("28 DROP");
    try expectEqual(@as(usize, 0), repl.stack.items.len);
}

test "SWAP" {
    var repl = Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line("1 2 SWAP");
    try expectEqual(@as(usize, 2), repl.stack.items.len);
    try expectEqual(@as(i32, 1), repl.stack.items[1]);
    try expectEqual(@as(i32, 2), repl.stack.items[0]);
}

test "OVER" {
    var repl = Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line("1 2 OVER");
    try expectEqual(@as(usize, 3), repl.stack.items.len);
    try expectEqual(@as(i32, 1), repl.stack.items[2]);
    try expectEqual(@as(i32, 2), repl.stack.items[1]);
    try expectEqual(@as(i32, 1), repl.stack.items[0]);
}

test "ROT" {
    var repl = Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line("1 2 3 ROT");
    try expectEqual(@as(usize, 3), repl.stack.items.len);
    try expectEqual(@as(i32, 1), repl.stack.items[0]);
    try expectEqual(@as(i32, 3), repl.stack.items[1]);
    try expectEqual(@as(i32, 2), repl.stack.items[2]);
}

test "+" {
    var repl = Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line("42 1 +");
    try expectEqual(@as(usize, 1), repl.stack.items.len);
    try expectEqual(@as(i32, 43), repl.stack.items[0]);
}

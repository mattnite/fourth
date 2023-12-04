const std = @import("std");
const testing = std.testing;
const assert = std.debug.assert;

const fourth = @import("fourth.zig");
const Repl = fourth.Repl;
const PrimitiveOptions = Repl.PrimitiveOptions;

pub const defaults = [_]PrimitiveOptions{
    .{
        .name = "PRIMITIVES",
        .signature = "( -- )",
        .description = "Print primitive documentation",
        .func = PRIMITIVES,
    },
    .{
        .name = "GLOSSARY",
        .signature = "( -- )",
        .description = "Print glossary table",
        .func = GLOSSARY,
    },

    //==========================================================================
    // Stack Manipulation
    //==========================================================================
    .{
        .name = "DUP",
        .signature = "( a -- a a )",
        .description = "Duplicate top of the stack",
        .func = DUP,
    },
    //.{ .name = "?DUP" },
    .{
        .name = "DROP",
        .signature = "( a -- )",
        .description = "Drop top of the stack",
        .func = DROP,
    },
    .{
        .name = "SWAP",
        .signature = "( -- )",
        .description = "Swap the two top entries of the stack",
        .func = SWAP,
    },
    .{
        .name = "OVER",
        .signature = "( a b -- a b a )",
        .description = "Copy the second entry of the stack and push it to the top",
        .func = OVER,
    },
    .{
        .name = "ROT",
        .signature = "( a b c -- c a b )",
        .description = "Rotate the top 3 entries of the stack",
        .func = ROT,
    },
    .{
        .name = "AND",
        .signature = "( a b -- c )",
        .description = "Bitwise and the top two entries and push result to the stack",
        .func = AND,
    },
    .{
        .name = "OR",
        .signature = "( a b -- c )",
        .description = "Bitwise or the top two entries and push result to the stack",
        .func = OR,
    },
    .{
        .name = "XOR",
        .signature = "( a b -- c )",
        .description = "Bitwise xor the top two entries and push result to the stack",
        .func = XOR,
    },

    //==========================================================================
    // Return Stack Manipulation
    //==========================================================================
    .{
        .name = ">R",
        .signature = "( a -- )",
        .description = "Push top of the stack onto the return stack",
        .func = @">R",
    },
    .{
        .name = "R>",
        .signature = "( -- a )",
        .description = "Push top of the return stack onto the stack",
        .func = @">R",
    },
    .{
        .name = "R@",
        .signature = "( -- a )",
        .description = "Copy top of the return stack onto the stack",
        .func = @"R@",
    },
    //==========================================================================
    // Variables
    //==========================================================================
    .{
        .name = "!",
        .signature = "( n addr -- )",
        .description = "Store number to addr",
        .func = @"!",
    },
    .{
        .name = "@",
        .signature = "( addr -- n )",
        .description = "Fetch value at addr",
        .func = @"@",
    },
    .{
        .name = "?",
        .signature = "( addr -- )",
        .description = "Prints contents of address followed by a space",
        .func = @"?",
    },
    .{
        .name = "+!",
        .signature = "( n addr -- )",
        .description = "Add number to contents of address",
        .func = @"+!",
    },
    //.{ .name = "C!" },
    //.{ .name = "C@" },
    //==========================================================================
    // Arrays
    //==========================================================================
    //.{ .name = "FILL" },
    //.{ .name = "ERASE" },
    //.{ .name = "ERASE" },

    //==========================================================================
    // Printing
    //==========================================================================
    .{
        .name = ".",
        .signature = "( a -- )",
        .description = "Pop and print the top of the stack",
        .func = @".",
    },
    .{
        .name = "CR",
        .signature = "( -- )",
        .description = "Print a carriage return",
        .func = CR,
    },
    .{
        .name = "EMIT",
        .signature = "( a -- )",
        .description = "Print number as ascii character",
        .func = EMIT,
    },

    //==========================================================================
    // Arithmetic
    //==========================================================================
    .{
        .name = "+",
        .signature = "( lhs rhs -- res )",
        .description = "Add the two top numbers on the stack and push the result",
        .func = @"+",
    },
    .{
        .name = "-",
        .signature = "( lhs rhs -- res )",
        .description = "Subtract rhs from lhs and push result to the stack",
        .func = @"-",
    },
    .{
        .name = "/",
        .signature = "( lhs rhs -- res )",
        .description = "Divide lhs by rhs and push result to the stack",
        .func = @"/",
    },
    //.{ .name = "/MOD" },
    //.{ .name = "MOD" },
    //.{ .name = "*/" },
    //.{ .name = "*/MOD" },
    //.{ .name = "1+" },
    //.{ .name = "1-" },
    //.{ .name = "2+" },
    //.{ .name = "2-" },
    //.{ .name = "2*" },
    //.{ .name = "2/" },
    //.{ .name = "ABS" },
    //.{ .name = "NEGATE" },
    //.{ .name = "MIN" },
    //.{ .name = "MAX" },

    //==========================================================================
    // Comparison
    //==========================================================================
    .{
        .name = "=",
        .signature = "( n1 n2 -- flag )",
        .description = "Returns true if n1 and n2 are equal",
        .func = @"=",
    },
    .{
        .name = "<>",
        .signature = "( n1 n2 -- flag )",
        .description = "Returns true if n1 and n2 are not equal",
        .func = @"<>",
    },
    .{
        .name = "<",
        .signature = "( n1 n2 -- flag )",
        .description = "Returns true if n1 is less than n2",
        .func = @"<",
    },
    .{
        .name = ">",
        .signature = "( n1 n2 -- flag )",
        .description = "Returns true if n1 is greater than n2",
        .func = @">",
    },
    .{
        .name = "U<",
        .signature = "( u1 u2 -- flag )",
        .description = "Returns true if u1 is greater than u2 (unsigned)",
        .func = @"U<",
    },
    .{
        .name = "U>",
        .signature = "( u1 u2 -- flag )",
        .description = "Returns true if u1 is greater than u2 (unsigned)",
        .func = @"U>",
    },
    .{
        .name = "0=",
        .signature = "( n -- flag )",
        .description = "Returns true if n is 0",
        .func = @"0=",
    },
    .{
        .name = "0<",
        .signature = "( n -- flag )",
        .description = "Returns true if n is negative",
        .func = @"0<",
    },
    .{
        .name = "0>",
        .signature = "( n -- flag )",
        .description = "Returns true if n is positive",
        .func = @"0>",
    },
};

fn print_with_padding(writer: anytype, string: ?[]const u8, max: usize) !void {
    var count: usize = if (string) |str| blk: {
        try writer.writeAll(str);
        break :blk str.len;
    } else 0;

    try writer.writeByteNTimes(' ', max - count);
}

const PrimitiveEntry = struct {
    word: []const u8,
    signature: ?[]const u8,
    description: ?[]const u8,

    fn print(entry: PrimitiveEntry, writer: anytype, max_word_len: usize, max_sig_len: usize, max_desc_len: usize) !void {
        try print_with_padding(writer, entry.word, max_word_len);
        try writer.writeByte(' ');
        try print_with_padding(writer, entry.signature, max_sig_len);
        try writer.writeByte(' ');
        try print_with_padding(writer, entry.description, max_desc_len);
        try writer.writeByte('\n');
    }
};

fn PRIMITIVES(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const stdout = std.io.getStdOut().writer();

    var entries = std.ArrayList(PrimitiveEntry).init(repl.gpa);
    defer entries.deinit();

    for (repl.primitives.keys()) |word_id| {
        const word = for (repl.words.values(), repl.words.keys()) |entry_word_id, word| {
            if (word_id == entry_word_id)
                break word;
        } else unreachable;

        try entries.append(.{
            .word = word,
            .signature = repl.signatures.get(word_id),
            .description = repl.descriptions.get(word_id),
        });
    }

    var max_word_len: usize = "WORD".len;
    var max_sig_len: usize = "SIGNATURE".len;
    var max_desc_len: usize = "DESCRIPTION".len;
    for (entries.items) |entry| {
        max_word_len = @max(max_word_len, entry.word.len);
        max_sig_len = @max(max_sig_len, if (entry.signature) |sig| sig.len else 0);
        max_desc_len = @max(max_desc_len, if (entry.description) |desc| desc.len else 0);
    }

    const header = PrimitiveEntry{
        .word = "WORD",
        .signature = "SIGNATURE",
        .description = "DESCRIPTION",
    };

    // TODO: alphabetize entries

    try stdout.writeByte('\n');
    try header.print(stdout, max_word_len, max_sig_len, max_desc_len);
    try stdout.writeByteNTimes('=', 2 + max_word_len + max_sig_len + max_desc_len);
    try stdout.writeByte('\n');
    for (entries.items) |entry|
        try entry.print(stdout, max_word_len, max_sig_len, max_desc_len);
}

const GlossaryEntry = struct { word: []const u8, definition: Repl.Definition };

fn get_word_from_id(repl: Repl, word_id: Repl.WordId) ?[]const u8 {
    return for (repl.words.values(), repl.words.keys()) |entry_word_id, word| {
        if (word_id == entry_word_id)
            break word;
    } else null;
}

fn GLOSSARY(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const stdout = std.io.getStdOut().writer();

    var list = std.ArrayList(GlossaryEntry).init(repl.gpa);
    defer list.deinit();

    var word_max_len = "WORD".len;
    for (repl.glossary.keys(), repl.glossary.values()) |word_id, definition| {
        const entry = GlossaryEntry{
            .word = get_word_from_id(repl.*, word_id).?,
            .definition = definition,
        };

        try list.append(entry);

        word_max_len = @max(entry.word.len, word_max_len);
    }

    try stdout.writeByte('\n');
    try print_with_padding(stdout, "WORD", word_max_len);
    try stdout.writeAll(" DEFINITION\n");

    try stdout.writeByteNTimes('=', 80);
    try stdout.writeByte('\n');

    for (list.items) |item| {
        try print_with_padding(stdout, item.word, word_max_len);
        try stdout.writeAll(" :");
        for (item.definition) |def_entry| {
            try stdout.writeByte(' ');
            switch (def_entry.tag) {
                .integer => try stdout.print("{}", .{def_entry.payload.integer}),
                .word => switch (def_entry.payload.word) {
                    .@":",
                    .@";",
                    .ABORT,
                    .FORGET,
                    .DEBUG,
                    .VARIABLE,
                    .CONSTANT,
                    .IF,
                    .THEN,
                    .ELSE,
                    .DO,
                    .LOOP,
                    .@"+LOOP",
                    .LEAVE,
                    .BEGIN,
                    .UNTIL,
                    .WHILE,
                    .REPEAT,
                    .AGAIN,
                    => inline for (@typeInfo(Repl.WordId).Enum.fields) |field| {
                        if (def_entry.payload.word == @field(Repl.WordId, field.name))
                            try stdout.writeAll(field.name);
                    },
                    _ => try stdout.print("{s}", .{get_word_from_id(repl.*, def_entry.payload.word).?}),
                },
            }
        }

        try stdout.writeAll(" ;\n");
    }
}

fn DUP(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    if (repl.stack.items.len == 0)
        return error.StackUnderflow;

    try repl.stack.append(repl.gpa, repl.stack.items[repl.stack.items.len - 1]);
}

fn DROP(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    _ = try repl.pop();
}

fn SWAP(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    if (repl.stack.items.len < 2)
        return error.StackUnderflow;

    const len = repl.stack.items.len;
    std.mem.swap(Repl.Integer, &repl.stack.items[len - 1], &repl.stack.items[len - 2]);
}

fn @"="(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const rhs = try repl.pop();
    const lhs = try repl.pop();
    try repl.stack.append(repl.gpa, if (lhs == rhs) -1 else 0);
}

fn @"<>"(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const rhs = try repl.pop();
    const lhs = try repl.pop();
    try repl.stack.append(repl.gpa, if (lhs != rhs) -1 else 0);
}

fn @"<"(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const rhs = try repl.pop();
    const lhs = try repl.pop();
    try repl.stack.append(repl.gpa, if (lhs < rhs) -1 else 0);
}

fn @">"(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const rhs = try repl.pop();
    const lhs = try repl.pop();
    try repl.stack.append(repl.gpa, if (lhs > rhs) -1 else 0);
}

fn @"U<"(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const rhs: u32 = @bitCast(try repl.pop());
    const lhs: u32 = @bitCast(try repl.pop());
    try repl.stack.append(repl.gpa, if (lhs < rhs) -1 else 0);
}

fn @"U>"(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const rhs: u32 = @bitCast(try repl.pop());
    const lhs: u32 = @bitCast(try repl.pop());
    try repl.stack.append(repl.gpa, if (lhs > rhs) -1 else 0);
}

fn @"0="(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const n = try repl.pop();
    try repl.stack.append(repl.gpa, if (n == 0) -1 else 0);
}

fn @"0<"(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const n = try repl.pop();
    try repl.stack.append(repl.gpa, if (n < 0) -1 else 0);
}

fn @"0>"(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const n = try repl.pop();
    try repl.stack.append(repl.gpa, if (n > 0) -1 else 0);
}

fn @"."(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const stdio = std.io.getStdOut().writer();
    try stdio.print("{}\n", .{try repl.pop()});
}

fn CR(_: *Repl, _: ?*anyopaque) Repl.Error!void {
    const stdio = std.io.getStdOut().writer();
    try stdio.writeAll("\r\n");
}

fn EMIT(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const stdio = std.io.getStdOut().writer();
    const n = try repl.pop();
    if (n < 0 or n > 255)
        return error.InvalidInput;

    try stdio.print("{c}\n", .{@as(u8, @intCast(n))});
}

fn @"+"(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const rhs = try repl.pop();
    const lhs = try repl.pop();
    try repl.stack.append(repl.gpa, lhs + rhs);
}

fn @"-"(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const rhs = try repl.pop();
    const lhs = try repl.pop();
    try repl.stack.append(repl.gpa, lhs - rhs);
}

fn @"*"(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const rhs = try repl.pop();
    const lhs = try repl.pop();
    try repl.stack.append(repl.gpa, lhs * rhs);
}

fn @"/"(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const denominator = try repl.pop();
    const numerator = try repl.pop();

    if (denominator == 0)
        return error.DivisionByZero;

    try repl.stack.append(repl.gpa, @divFloor(numerator, denominator));
}

fn OVER(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    if (repl.stack.items.len < 2)
        return error.StackUnderflow;

    try repl.stack.append(repl.gpa, repl.stack.items[repl.stack.items.len - 2]);
}

fn ROT(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    if (repl.stack.items.len < 3)
        return error.StackUnderflow;

    const popped = repl.stack.orderedRemove(repl.stack.items.len - 2);
    try repl.stack.append(repl.gpa, popped);
}

fn AND(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const rhs = try repl.pop();
    const lhs = try repl.pop();

    try repl.stack.append(repl.gpa, rhs & lhs);
}

fn OR(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const rhs = try repl.pop();
    const lhs = try repl.pop();

    try repl.stack.append(repl.gpa, rhs | lhs);
}

fn XOR(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const rhs = try repl.pop();
    const lhs = try repl.pop();

    try repl.stack.append(repl.gpa, rhs ^ lhs);
}

fn @"!"(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const index = try repl.pop();
    const n = try repl.pop();

    if (index >= repl.variables_buf.items.len)
        return error.OutOfBounds;

    repl.variables_buf.items[@intCast(index)] = n;
}

fn @"@"(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const index = try repl.pop();

    if (index >= repl.variables_buf.items.len)
        return error.OutOfBounds;

    try repl.push(repl.variables_buf.items[@intCast(index)]);
}

fn @"?"(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const index = try repl.pop();
    if (index >= repl.variables_buf.items.len)
        return error.OutOfBounds;

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{} ", .{repl.variables_buf.items[@intCast(index)]});
}

fn @"+!"(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const index = try repl.pop();
    const n = try repl.pop();

    if (index >= repl.variables_buf.items.len)
        return error.OutOfBounds;

    repl.variables_buf.items[@intCast(index)] += n;
}

//==========================================================================
// The Return Stack
//==========================================================================
fn @">R"(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const n = try repl.pop();
    try repl.push_return(n);
}

fn @"R>"(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    const n = try repl.pop_return();
    try repl.push(n);
}

/// copy top of return stack onto stack
fn @"R@"(repl: *Repl, _: ?*anyopaque) Repl.Error!void {
    try repl.push(repl.return_stack.items[repl.return_stack.items.len - 1]);
}

//==============================================================================
// Tests
//==============================================================================

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectError = std.testing.expectError;

test "DUP" {
    var repl = try Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line("28 DUP");
    try expectEqual(@as(usize, 2), repl.stack.items.len);
    try expectEqual(@as(i32, 28), repl.stack.items[0]);
    try expectEqual(@as(i32, 28), repl.stack.items[1]);
}

test "DROP" {
    var repl = try Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line("28 DROP");
    try expectEqual(@as(usize, 0), repl.stack.items.len);
}

test "SWAP" {
    var repl = try Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line("1 2 SWAP");
    try expectEqual(@as(usize, 2), repl.stack.items.len);
    try expectEqual(@as(i32, 1), repl.stack.items[1]);
    try expectEqual(@as(i32, 2), repl.stack.items[0]);
}

test "OVER" {
    var repl = try Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line("1 2 OVER");
    try expectEqual(@as(usize, 3), repl.stack.items.len);
    try expectEqual(@as(i32, 1), repl.stack.items[2]);
    try expectEqual(@as(i32, 2), repl.stack.items[1]);
    try expectEqual(@as(i32, 1), repl.stack.items[0]);
}

test "ROT" {
    var repl = try Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line("1 2 3 ROT");
    try expectEqual(@as(usize, 3), repl.stack.items.len);
    try expectEqual(@as(i32, 1), repl.stack.items[0]);
    try expectEqual(@as(i32, 3), repl.stack.items[1]);
    try expectEqual(@as(i32, 2), repl.stack.items[2]);
}

test "+" {
    var repl = try Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line("42 1 +");
    try expectEqual(@as(usize, 1), repl.stack.items.len);
    try expectEqual(@as(i32, 43), repl.stack.items[0]);
}

test "=" {
    var repl = try Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line("42 1 =");
    try repl.feed_line("3 3 =");

    try expectEqual(@as(usize, 2), repl.stack.items.len);
    try expectEqual(@as(i32, 0), repl.stack.items[0]);
    try expectEqual(@as(i32, -1), repl.stack.items[1]);
}

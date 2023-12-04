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

    constants: std.AutoArrayHashMapUnmanaged(WordId, usize) = .{},
    constants_buf: std.ArrayListUnmanaged(Integer) = .{},
    variables: std.AutoArrayHashMapUnmanaged(WordId, usize) = .{},
    variables_buf: std.ArrayListUnmanaged(Integer) = .{},

    glossary: std.AutoArrayHashMapUnmanaged(WordId, Definition) = .{},
    compilations: std.AutoArrayHashMapUnmanaged(WordId, u32) = .{},
    instructions: std.ArrayListUnmanaged(u8) = .{},
    stack: std.ArrayListUnmanaged(Integer) = .{},
    return_stack: std.ArrayListUnmanaged(Integer) = .{},
    loop_stack: std.ArrayListUnmanaged(LoopContext) = .{},
    count: u32,
    current_variable: ?usize = null,

    state: State,

    const State = union(enum) {
        execute,
        constant_start,
        variable_start,
        definition_start,
        definition_body: struct {
            word: []const u8,
            buffer: std.ArrayListUnmanaged(Entity),
        },

        fn deinit(state: *State, allocator: std.mem.Allocator) void {
            switch (state.*) {
                .execute, .constant_start, .variable_start, .definition_start => {},
                .definition_body => |*body| {
                    body.buffer.deinit(allocator);
                    allocator.free(body.word);
                },
            }
        }

        fn transition_to(old_state: *State, allocator: std.mem.Allocator, new_state: State) void {
            old_state.deinit(allocator);
            old_state.* = new_state;
        }

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
                .constant_start => try writer.writeAll("CONSTANT_START"),
                .variable_start => try writer.writeAll("VARIABLE_START"),
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
        OutOfBounds,
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
    pub const Definition = []const Entity;
    const Entity = struct {
        payload: union {
            integer: Integer,
            word: WordId,
        },
        tag: Tag,

        const Tag = enum(u1) {
            integer,
            word,
        };

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

    const InstructionTag = enum(u8) {
        /// Return from word
        ret,
        /// Execute word
        word,
        // Jump by offset
        jmp,
        /// Pop stack and jump by <offset> if it's true
        jmp_if_true,
        /// Pop stack and jump by <offset> if it's false
        jmp_if_false,
        /// Increment loop index by 1. Jump if index is equal to or greater
        /// than limit (TODO: greater than limit too?)
        jmp_loop_inc,
        /// Increment loop index by popping off the stack. Jump if index is
        /// equal to or greater than limit (TODO: greater than limit too?)
        jmp_loop_inc_pop,
        /// Pushes integer literal onto stack
        push,
        /// Pushes limit and index from stack onto loop stack
        loop_push,
    };

    const LoopContext = struct {
        index: Integer,
        limit: Integer,
    };

    const JumpOffset = i16;

    const Instruction = union(InstructionTag) {
        ret,
        word: WordId,
        jmp: JumpOffset,
        jmp_if_true: JumpOffset,
        jmp_if_false: JumpOffset,
        jmp_loop_inc: JumpOffset,
        jmp_loop_inc_pop: JumpOffset,
        push: Integer,
        loop_push,

        fn read(reader: anytype) !Instruction {
            const tag = try reader.readEnum(InstructionTag, .Little);
            return switch (tag) {
                .ret => .{ .ret = {} },
                .word => blk: {
                    const word_id: u32 = try reader.readIntLittle(u32);
                    break :blk .{ .word = @enumFromInt(word_id) };
                },
                .jmp => .{ .jmp = try reader.readIntLittle(JumpOffset) },
                .jmp_if_true => .{ .jmp_if_true = try reader.readIntLittle(JumpOffset) },
                .jmp_if_false => .{ .jmp_if_false = try reader.readIntLittle(JumpOffset) },
                .jmp_loop_inc => .{ .jmp_loop_inc = try reader.readIntLittle(JumpOffset) },
                .jmp_loop_inc_pop => .{ .jmp_loop_inc_pop = try reader.readIntLittle(JumpOffset) },
                .push => .{ .push = try reader.readIntLittle(Integer) },
                .loop_push => .{ .loop_push = {} },
            };
        }

        fn write(insn: Instruction, writer: anytype) !void {
            const tag: u8 = @intFromEnum(insn);
            try writer.writeIntLittle(u8, tag);
            switch (insn) {
                .ret => {},
                .word => |word_id| {
                    const id: u32 = @intFromEnum(word_id);
                    try writer.writeIntLittle(u32, id);
                },
                .jmp => |offset| try writer.writeIntLittle(JumpOffset, offset),
                .jmp_if_true => |offset| try writer.writeIntLittle(JumpOffset, offset),
                .jmp_if_false => |offset| try writer.writeIntLittle(JumpOffset, offset),
                .jmp_loop_inc => |offset| try writer.writeIntLittle(JumpOffset, offset),
                .jmp_loop_inc_pop => |offset| try writer.writeIntLittle(JumpOffset, offset),
                .push => |integer| try writer.writeIntLittle(Integer, integer),
                .loop_push => {},
            }
        }

        fn get_size(insn: Instruction) u32 {
            return 1 + switch (insn) {
                .ret => @as(u32, 0),
                .word => @sizeOf(WordId),
                .jmp => @sizeOf(JumpOffset),
                .jmp_if_true => @sizeOf(JumpOffset),
                .jmp_if_false => @sizeOf(JumpOffset),
                .jmp_loop_inc => @sizeOf(JumpOffset),
                .jmp_loop_inc_pop => @sizeOf(JumpOffset),
                .push => @sizeOf(Integer),
                .loop_push => @as(u32, 0),
            };
        }

        pub fn format(
            insn: Instruction,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;

            switch (insn) {
                .ret => try writer.writeAll("ret"),
                .word => |word_id| try writer.print("word {}", .{word_id}),
                .jmp => |offset| try writer.print("jmp {}", .{offset}),
                .jmp_if_true => |offset| try writer.print("jmp_if_true {}", .{offset}),
                .jmp_if_false => |offset| try writer.print("jmp_if_false {}", .{offset}),
                .jmp_loop_inc => |offset| try writer.print("jmp_loop_inc {}", .{offset}),
                .jmp_loop_inc_pop => |offset| try writer.print("jump_loop_inc_pop {}", .{offset}),
                .push => |integer| try writer.print("push {}", .{integer}),
                .loop_push => try writer.writeAll("loop_push"),
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
        ABORT,
        FORGET,
        DEBUG,
        VARIABLE,
        CONSTANT,

        // =====================================================================
        // Branching logic
        // =====================================================================

        /// ( flag -- )
        ///
        /// If flag is true (non-zero) executes xxx; otherwise executes yyy;
        /// continues execution with zzz. The phrase ELSE yyy is optional.
        ///   IF xxx THEN zzz
        ///   IF xxx ELSE yyy THEN zzz
        IF,
        THEN,
        ELSE,
        /// ( limit index — )
        ///
        /// Sets up a finite loop, given the index and limit. Usage:
        ///   DO ... LOOP
        ///   DO ... n +LOOP
        DO,
        /// ( -- )
        ///
        /// Add one to the loop index. If the loop index is then equal to the
        /// loop limit, discard the loop parameters and continue execution
        /// immediately following LOOP. Otherwise continue execution at the
        /// beginning of the loop (after DO).
        LOOP,
        /// ( n -- )
        ///
        /// Add n to the loop index. If the loop index did not cross the
        /// boundary between the loop limit minus one and the loop limit,
        /// continue execution at the beginning of the loop. Otherwise, discard
        /// the current loop control parameters and continue execution
        /// immediately following the loop (after DO).
        @"+LOOP",
        /// ( -- )
        ///
        /// Terminate the loop immediately
        LEAVE,
        /// ( -- )
        ///
        /// Marks the start of an indefinite loop. Usage:
        ///   BEGIN ... flag UNTIL
        ///   BEGIN ... flag WHILE ... REPEAT
        ///   BEGIN ... AGAIN
        BEGIN,
        /// ( flag -- )
        ///
        /// If flag is false, go back to BEGIN. If flag is true, terminate the
        /// loop.
        UNTIL,
        AGAIN,
        /// ( flag -- )
        ///
        /// If flag is true, continue. If flag is false, terminate the loop
        /// (after REPEAT).
        WHILE,
        /// ( -- )
        ///
        /// Resolves forward branch from WHILE; goes back to BEGIN.
        REPEAT,
        /// TODO:
        /// ( -- )
        ///
        /// Terminates execution for the current task and returns control to
        /// the terminal.
        // QUIT,

        // Runtime word IDs
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

        repl.instructions.deinit(repl.gpa);

        repl.primitives.deinit(repl.gpa);
        repl.contexts.deinit(repl.gpa);
        repl.signatures.deinit(repl.gpa);
        repl.descriptions.deinit(repl.gpa);

        repl.compilations.deinit(repl.gpa);
        repl.glossary.deinit(repl.gpa);
        repl.stack.deinit(repl.gpa);
        repl.return_stack.deinit(repl.gpa);
        repl.loop_stack.deinit(repl.gpa);
    }

    fn dump_state(repl: *Repl) !void {
        const stderr = std.io.getStdErr().writer();
        try stderr.writeByte('\n');
        try stderr.print("state: {}\n", .{repl.state});
        try stderr.print("total instruction space: {}\n", .{repl.instructions.items.len});

        try stderr.writeAll("stack:\n");

        var it = std.mem.reverseIterator(repl.stack.items);
        while (it.next()) |stack_item|
            try stderr.print("  {}\n", .{stack_item});

        // TODO: log allocated insn space
        try stderr.writeByte('\n');
    }

    fn create_word_id(repl: *Repl) WordId {
        const word_id: WordId = @enumFromInt(@typeInfo(WordId).Enum.fields.len + repl.count);
        repl.count += 1;
        return word_id;
    }

    fn get_word_id(repl: *Repl, token: []const u8) ?WordId {
        return std.meta.stringToEnum(WordId, token) orelse repl.words.get(token);
    }

    fn load_insn(repl: Repl, pc: u32) !Instruction {
        var fbs = std.io.fixedBufferStream(@as([]const u8, repl.instructions.items));
        try fbs.seekTo(pc);

        return Instruction.read(fbs.reader());
    }

    fn apply_jump(pc: u32, offset: JumpOffset) !u32 {
        return if (offset < 0)
            pc - @as(u32, @intCast(try std.math.absInt(offset)))
        else
            pc + @as(u32, @intCast(offset));
    }

    fn execute_word(repl: *Repl, word_id: WordId) !void {
        var pc: u32 = repl.compilations.get(word_id).?;
        while (true) {
            const insn = try repl.load_insn(pc);
            std.log.debug("pc: {}, insn: {}", .{ pc, insn });
            switch (insn) {
                .jmp => |offset| pc = try apply_jump(pc, offset),
                .jmp_if_false => |offset| {
                    // jump if zero
                    const cond = try repl.pop();
                    if (!is_boolean(cond))
                        return error.NotABoolean;

                    if (cond == 0)
                        pc = try apply_jump(pc, offset)
                    else
                        pc += insn.get_size();
                },
                .jmp_if_true => |offset| {
                    // jump if zero
                    const cond = try repl.pop();
                    if (!is_boolean(cond))
                        return error.NotABoolean;

                    if (cond == -1)
                        pc = try apply_jump(pc, offset)
                    else
                        pc += insn.get_size();
                },
                .jmp_loop_inc => |offset| {
                    const top = &repl.loop_stack.items[repl.loop_stack.items.len - 1];
                    top.index +%= 1;

                    if (top.index < top.limit) {
                        pc = try apply_jump(pc, offset);
                    } else {
                        _ = repl.loop_stack.pop();
                        pc += insn.get_size();
                    }
                },
                .jmp_loop_inc_pop => |offset| {
                    const top = &repl.loop_stack.items[repl.loop_stack.items.len - 1];
                    top.index +%= try repl.pop();

                    if (top.index < top.limit) {
                        pc = try apply_jump(pc, offset);
                    } else {
                        _ = repl.loop_stack.pop();
                        pc += insn.get_size();
                    }
                },
                .word => |next_word_id| {
                    if (repl.primitives.get(next_word_id)) |prim| {
                        try prim(repl, repl.contexts.get(word_id));
                    } else if (repl.constants.get(word_id)) |index| {
                        try repl.push(repl.constants_buf.items[index]);
                    } else if (repl.variables.get(word_id)) |index| {
                        try repl.push(@intCast(index));
                    } else {
                        try repl.push_return(@intCast(pc + insn.get_size()));
                        pc = repl.compilations.get(next_word_id).?;
                        continue;
                    }

                    pc += insn.get_size();
                },
                .push => |integer| {
                    try repl.push(integer);
                    pc += insn.get_size();
                },
                .loop_push => {
                    const index = try repl.pop();
                    const limit = try repl.pop();
                    try repl.loop_stack.append(repl.gpa, .{
                        .index = index,
                        .limit = limit,
                    });
                    pc += insn.get_size();
                },
                .ret => {
                    if (repl.return_stack.items.len == 0) {
                        return;
                    }

                    pc = @intCast(try repl.pop_return());
                },
            }
        }
    }

    fn calculate_jump_offset(insns: []const Instruction) JumpOffset {
        var ret: JumpOffset = 0;
        for (insns) |insn| ret += @intCast(insn.get_size());
        return ret;
    }

    const FindEntityOptions = struct {
        increase_depth: WordId,
        decrease_depth: []const WordId,
        look_for: []const WordId,
    };

    fn find_next_entity(entities: []const Entity, start_index: u32, opts: FindEntityOptions) !u32 {
        var depth: u32 = 0;
        return for (entities, start_index..) |entity, i| {
            if (entity.tag == .integer)
                continue;

            const other_word = entity.payload.word;
            if (other_word == opts.increase_depth) {
                depth += 1;
                continue;
            }

            if (depth == 0) {
                for (opts.look_for) |word|
                    if (word == other_word)
                        return @intCast(i);
            }

            const decrease = for (opts.decrease_depth) |word| {
                if (word == other_word)
                    break true;
            } else false;

            if (decrease) {
                depth -= 1;
                continue;
            }
        } else error.NoEndingFound;
    }

    const BranchContext = struct {
        variant: union(enum) {
            IF: struct {
                if_jmp_idx: u32,
                else_jmp_idx: ?u32 = null,
            },
            DO_LOOP: u32,
            @"DO_+LOOP": u32,
            BEGIN_UNTIL: u32,
            BEGIN_AGAIN: u32,
            BEGIN_REPEAT: struct {
                jmp_to: u32,
                while_idx: ?u32 = null,
            },

            fn is_loop(variant: @This()) bool {
                return variant != .IF;
            }
        },
        // this is a list of encountered LEAVE words at this branch nesting, it
        // is only valid for loops. We need to go back and update the jump
        // offsets so they skip out of the loop.
        leaves: std.ArrayListUnmanaged(u32) = .{},
    };

    fn process_leaves(leaves: []const u32, insns: []Instruction) void {
        for (leaves) |leave_idx| {
            const leave_offset = calculate_jump_offset(insns[leave_idx..]);
            insns[leave_idx].jmp = leave_offset;
            std.log.info("leave_idx: {}, leave_offset: {}", .{ leave_idx, leave_offset });
        }
    }

    fn compile(repl: *Repl, word: []const u8, definition: Definition) !void {
        var insns = std.ArrayList(Instruction).init(repl.gpa);
        defer insns.deinit();

        var ctxs = std.ArrayList(BranchContext).init(repl.gpa);
        defer {
            for (ctxs.items) |*ctx| ctx.leaves.deinit(repl.gpa);
            ctxs.deinit();
        }

        for (definition, 0..) |entity, i| switch (entity.tag) {
            .integer => try insns.append(.{ .push = entity.payload.integer }),
            .word => switch (entity.payload.word) {
                .@":",
                .@";",
                .ABORT,
                .FORGET,
                .DEBUG,
                .VARIABLE,
                .CONSTANT,
                => return error.TODO,
                .IF => {
                    // Find corresponding ELSE, or THEN if there's no else, that's where we're jumping
                    const next = try find_next_entity(definition[i + 1 ..], @intCast(i + 1), .{
                        .increase_depth = .IF,
                        .decrease_depth = &.{.THEN},
                        .look_for = &.{ .ELSE, .THEN },
                    });
                    _ = next;

                    // TODO: ensure that there is a corresponding THEN

                    try ctxs.append(.{
                        .variant = .{
                            .IF = .{
                                .if_jmp_idx = @intCast(insns.items.len),
                            },
                        },
                    });

                    try insns.append(.{ .jmp_if_false = 0 });
                },
                .ELSE => {
                    // TODO: gracefully handle bad syntax
                    const ctx = &ctxs.items[ctxs.items.len - 1];
                    assert(ctx.variant == .IF);

                    // Calculate jump offset for IF
                    const if_jmp_idx = ctx.variant.IF.if_jmp_idx;

                    // Find corresponding THEN, that's where we're jumping
                    const next = try find_next_entity(definition[i + 1 ..], @intCast(i + 1), .{
                        .increase_depth = .IF,
                        .decrease_depth = &.{.THEN},
                        .look_for = &.{.THEN},
                    });
                    _ = next;

                    ctx.variant.IF.else_jmp_idx = @intCast(insns.items.len);
                    try insns.append(.{ .jmp = 0 });

                    const offset = calculate_jump_offset(insns.items[if_jmp_idx..]);
                    insns.items[if_jmp_idx].jmp_if_false = offset;
                },
                .THEN => {
                    // calculate jump offset for IF or ELSE
                    const ctx = &ctxs.items[ctxs.items.len - 1];
                    if (ctx.variant.IF.else_jmp_idx) |else_jmp_idx| {
                        const offset = calculate_jump_offset(insns.items[else_jmp_idx..]);
                        insns.items[else_jmp_idx] = .{
                            .jmp = offset,
                        };
                    } else {
                        const if_jmp_idx = ctx.variant.IF.if_jmp_idx;
                        const offset = calculate_jump_offset(insns.items[ctx.variant.IF.if_jmp_idx..]);
                        insns.items[if_jmp_idx] = .{
                            .jmp_if_false = offset,
                        };
                    }

                    _ = ctxs.pop();
                },
                .LEAVE => {
                    std.log.info("leave: {}", .{i});
                    // walk down the stack until we find the first loop
                    var it = std.mem.reverseIterator(ctxs.items);
                    const ctx: *BranchContext = while (it.next()) |*ctx| {
                        if (ctx.variant.is_loop())
                            break &ctxs.items[it.index];
                    } else return error.NoLoopFound;
                    try ctx.leaves.append(repl.gpa, @intCast(insns.items.len));
                    try insns.append(.{
                        .jmp = 0,
                    });
                },
                .DO => {
                    try insns.append(.{ .loop_push = {} });
                    const jmp_to: u32 = @intCast(insns.items.len);
                    const next = try find_next_entity(definition[i + 1 ..], @intCast(i + 1), .{
                        .increase_depth = .DO,
                        .decrease_depth = &.{ .LOOP, .@"+LOOP" },
                        .look_for = &.{ .LOOP, .@"+LOOP" },
                    });

                    try ctxs.append(.{
                        .variant = switch (definition[next].payload.word) {
                            .LOOP => .{ .DO_LOOP = jmp_to },
                            .@"+LOOP" => .{ .@"DO_+LOOP" = jmp_to },
                            else => unreachable,
                        },
                    });
                },
                .LOOP, .@"+LOOP" => {
                    const ctx = &ctxs.items[ctxs.items.len - 1];
                    const jmp_idx = switch (entity.payload.word) {
                        .LOOP => ctx.variant.DO_LOOP,
                        .@"+LOOP" => ctx.variant.@"DO_+LOOP",
                        else => unreachable,
                    };

                    {
                        const offset = -calculate_jump_offset(insns.items[jmp_idx..]);
                        try insns.append(switch (entity.payload.word) {
                            .LOOP => .{
                                .jmp_loop_inc = offset,
                            },
                            .@"+LOOP" => .{
                                .jmp_loop_inc_pop = offset,
                            },
                            else => unreachable,
                        });
                    }

                    process_leaves(ctx.leaves.items, insns.items);

                    _ = ctxs.pop();
                },
                .BEGIN => {
                    const jmp_to: u32 = @intCast(insns.items.len);
                    const next = try find_next_entity(definition[i + 1 ..], @intCast(i + 1), .{
                        .increase_depth = .BEGIN,
                        .decrease_depth = &.{ .UNTIL, .AGAIN, .REPEAT },
                        .look_for = &.{ .UNTIL, .AGAIN, .REPEAT },
                    });

                    try ctxs.append(.{
                        .variant = switch (definition[next].payload.word) {
                            .UNTIL => .{
                                .BEGIN_UNTIL = jmp_to,
                            },
                            .AGAIN => .{
                                .BEGIN_AGAIN = jmp_to,
                            },
                            .REPEAT => .{
                                .BEGIN_REPEAT = .{
                                    .jmp_to = jmp_to,
                                },
                            },
                            else => unreachable,
                        },
                    });
                },
                .UNTIL, .AGAIN => {
                    const ctx = &ctxs.items[ctxs.items.len - 1];
                    const jmp_idx = switch (entity.payload.word) {
                        .UNTIL => ctx.variant.BEGIN_UNTIL,
                        .AGAIN => ctx.variant.BEGIN_AGAIN,
                        else => unreachable,
                    };

                    const offset = -calculate_jump_offset(insns.items[jmp_idx..]);
                    try insns.append(switch (entity.payload.word) {
                        .UNTIL => .{
                            .jmp_if_true = offset,
                        },
                        .AGAIN => .{
                            .jmp = offset,
                        },
                        else => unreachable,
                    });

                    process_leaves(ctx.leaves.items, insns.items);

                    _ = ctxs.pop();
                },
                .WHILE => {
                    const ctx = &ctxs.items[ctxs.items.len - 1];
                    ctx.variant.BEGIN_REPEAT.while_idx = @intCast(insns.items.len);
                    try insns.append(.{ .jmp_if_false = 0 });
                },
                .REPEAT => {
                    const ctx = &ctxs.items[ctxs.items.len - 1];
                    const begin_jmp_idx = ctx.variant.BEGIN_REPEAT.jmp_to;
                    const begin_offset = -calculate_jump_offset(insns.items[begin_jmp_idx..]);
                    try insns.append(.{
                        .jmp = begin_offset,
                    });

                    const while_jmp_idx = ctx.variant.BEGIN_REPEAT.while_idx.?;
                    const while_offset = calculate_jump_offset(insns.items[while_jmp_idx..]);
                    insns.items[while_jmp_idx].jmp_if_false = while_offset;

                    process_leaves(ctx.leaves.items, insns.items);

                    _ = ctxs.pop();
                },
                _ => try insns.append(.{ .word = entity.payload.word }),
            },
        };

        try insns.append(.{ .ret = {} });

        // TODO: backtracking?

        const compilation_index: u32 = @intCast(repl.instructions.items.len);

        const word_id = if (repl.words.get(word)) |word_id|
            word_id
        else word_id: {
            const word_id = repl.create_word_id();

            const word_copy = try repl.gpa.dupe(u8, word);
            errdefer repl.gpa.free(word_copy);

            try repl.words.putNoClobber(repl.gpa, word_copy, word_id);
            errdefer _ = repl.words.swapRemove(word_copy);

            break :word_id word_id;
        };

        const writer = repl.instructions.writer(repl.gpa);
        errdefer {
            for (compilation_index..repl.instructions.items.len) |_|
                _ = insns.orderedRemove(compilation_index);
        }

        var addr: u32 = 0;
        for (insns.items) |insn| {
            std.log.debug("{}: {}", .{ addr, insn });
            try insn.write(writer);
            addr += insn.get_size();
            switch (insn) {
                .ret, .word, .push, .loop_push => {},
                .jmp => |offset| assert(offset != 0),
                .jmp_if_true => |offset| assert(offset != 0),
                .jmp_if_false => |offset| assert(offset != 0),
                .jmp_loop_inc => |offset| assert(offset != 0),
                .jmp_loop_inc_pop => |offset| assert(offset != 0),
            }
        }

        const definition_copy = try repl.gpa.dupe(Entity, definition);
        errdefer repl.gpa.free(definition_copy);

        try repl.glossary.put(repl.gpa, word_id, definition_copy);
        errdefer _ = repl.glossary.swapRemove(word_id);

        try repl.compilations.put(repl.gpa, word_id, compilation_index);
    }

    fn feed_token(repl: *Repl, token: []const u8) !void {
        switch (repl.state) {
            .execute => {
                if (std.meta.stringToEnum(WordId, token)) |keyword| switch (keyword) {
                    .@":" => {
                        repl.state.transition_to(repl.gpa, .{ .definition_start = {} });
                        return;
                    },
                    .CONSTANT => {
                        repl.state.transition_to(repl.gpa, .{ .constant_start = {} });
                        return;
                    },
                    .VARIABLE => {
                        repl.state.transition_to(repl.gpa, .{ .variable_start = {} });
                        return;
                    },
                    .@";" => return error.NotDefiningFunction,
                    .IF, .THEN, .ELSE, .DO, .LOOP, .@"+LOOP", .LEAVE, .BEGIN, .UNTIL, .AGAIN, .WHILE, .REPEAT => return error.InvalidWord,
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
            .constant_start => {
                defer repl.state.transition_to(repl.gpa, .{ .execute = {} });

                if (std.meta.stringToEnum(WordId, token)) |keyword| switch (keyword) {
                    .@";", .@":", .ABORT, .CONSTANT, .VARIABLE, .FORGET => return error.TODO,
                    .IF, .THEN, .ELSE, .DO, .LOOP, .@"+LOOP", .LEAVE, .BEGIN, .UNTIL, .AGAIN, .WHILE, .REPEAT => return error.InvalidWord,
                    .DEBUG => return try repl.dump_state(),
                    _ => {},
                };

                // TODO: if this fails, should we push the value back onto the stack?
                const value = try repl.pop();
                try repl.add_constant(token, value);
            },
            .variable_start => {
                if (std.meta.stringToEnum(WordId, token)) |keyword| switch (keyword) {
                    .@";", .@":", .ABORT, .CONSTANT, .VARIABLE, .FORGET => return error.TODO,
                    .IF, .THEN, .ELSE, .DO, .LOOP, .@"+LOOP", .LEAVE, .BEGIN, .UNTIL, .AGAIN, .WHILE, .REPEAT => return error.InvalidWord,
                    .DEBUG => return try repl.dump_state(),
                    _ => {},
                };

                if (repl.words.contains(token))
                    return error.WordExists;

                const word_id = repl.create_word_id();
                const name = try repl.gpa.dupe(u8, token);
                errdefer repl.gpa.free(name);

                try repl.words.put(repl.gpa, name, word_id);
                errdefer _ = repl.words.swapRemove(name);

                try repl.variables.put(repl.gpa, word_id, repl.variables_buf.items.len);
                errdefer _ = repl.variables.swapRemove(word_id);

                try repl.variables_buf.append(repl.gpa, 0);
                errdefer _ = repl.variables_buf.pop();

                repl.state.transition_to(repl.gpa, .{ .execute = {} });
            },
            .definition_start => {
                if (std.meta.stringToEnum(WordId, token)) |keyword| switch (keyword) {
                    .@":" => return error.AlreadyStartedDefinition,
                    // treat as an exit out of defining a word
                    .ABORT, .@";" => {
                        repl.state.transition_to(repl.gpa, .{ .execute = {} });
                        return;
                    },
                    .CONSTANT, .VARIABLE => return error.InvalidWord,
                    .IF, .THEN, .ELSE, .DO, .LOOP, .@"+LOOP", .LEAVE, .BEGIN, .UNTIL, .AGAIN, .WHILE, .REPEAT => return error.InvalidWord,
                    .FORGET => return error.TODO,
                    .DEBUG => return try repl.dump_state(),
                    _ => {},
                };

                // TODO: name cannot be a number
                //if (std.fmt.parseInt(i32, token, 0)) {
                //    return error.WordIsANumber;
                //} else {}

                repl.state.transition_to(repl.gpa, .{
                    .definition_body = .{
                        .word = try repl.gpa.dupe(u8, token),
                        .buffer = std.ArrayListUnmanaged(Entity){},
                    },
                });
            },
            .definition_body => |*state| {
                errdefer repl.state.transition_to(repl.gpa, .{ .execute = {} });

                if (std.meta.stringToEnum(WordId, token)) |keyword| switch (keyword) {
                    .@":" => return error.AlreadyStartedDefinition,
                    // treat as an exit out of defining a word
                    .ABORT => {
                        repl.state.transition_to(repl.gpa, .{ .execute = {} });
                        return;
                    },
                    .@";" => {
                        try repl.compile(state.word, state.buffer.items);
                        // TODO: failure cleanup
                        repl.state.transition_to(repl.gpa, .{ .execute = {} });
                        return;
                    },
                    .CONSTANT, .VARIABLE => return error.InvalidWord,
                    .LEAVE, .IF, .THEN, .ELSE, .DO, .LOOP, .@"+LOOP", .BEGIN, .UNTIL, .AGAIN, .WHILE, .REPEAT => {},
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

    pub fn add_constant(repl: *Repl, name: []const u8, value: i32) !void {
        if (repl.words.contains(name))
            return error.WordExists;

        const word_id = repl.create_word_id();
        const name_copy = try repl.gpa.dupe(u8, name);
        errdefer repl.gpa.free(name_copy);

        try repl.words.put(repl.gpa, name_copy, word_id);
        errdefer _ = repl.words.swapRemove(name_copy);

        try repl.constants.put(repl.gpa, word_id, repl.constants_buf.items.len);
        errdefer _ = repl.constants.swapRemove(word_id);

        try repl.constants_buf.append(repl.gpa, value);
        errdefer _ = repl.constants_buf.pop();
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

test "if missing then" {
    var repl = try Repl.init(std.testing.allocator);
    defer repl.deinit();

    try expectError(error.NoEndingFound, repl.feed_line(": ?FULL  12 = IF 42 ;"));
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

    try repl.feed_line(": ?FULL  12 = IF 42 ELSE 50 THEN ;");
    try repl.feed_line("2 ?FULL");
    try expectEqual(@as(usize, 1), repl.stack.items.len);
    try expectEqual(@as(i32, 50), repl.stack.items[0]);

    try repl.feed_line("12 ?FULL");
    try expectEqual(@as(usize, 2), repl.stack.items.len);
    try expectEqual(@as(i32, 42), repl.stack.items[1]);
}

test "definition with nested if else" {
    var repl = try Repl.init(std.testing.allocator);
    defer repl.deinit();

    // TODO: comment: ( n -- )
    try repl.feed_line(": EGGSIZE                    ");
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

    try std.testing.expectEqualSlices(i32, &.{
        10, 1,
        19, 2,
        23, 3,
        25, 4,
        29, 5,
        50, 42,
    }, repl.stack.items);
}

//==============================================================================
// Loops
//==============================================================================

test "DO ... LOOP" {
    var repl = try Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line(": foo 10 0 DO 3 LOOP ;");
    try repl.feed_line("foo");
    try expectEqual(@as(usize, 10), repl.stack.items.len);
}

test "DO ... +LOOP" {
    var repl = try Repl.init(std.testing.allocator);
    defer repl.deinit();

    try repl.feed_line(": foo 10 0 DO 3 3 +LOOP ;");
    try repl.feed_line("foo");
    try expectEqual(@as(usize, 4), repl.stack.items.len);
}

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

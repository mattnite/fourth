const std = @import("std");
const fourth = @import("fourth.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var repl = try fourth.Repl.init(gpa.allocator());
    defer repl.deinit();

    var line_buf = std.ArrayList(u8).init(gpa.allocator());
    defer line_buf.deinit();

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    while (true) : (line_buf.clearRetainingCapacity()) {
        stdin.streamUntilDelimiter(line_buf.writer(), '\n', null) catch |err| {
            if (err == error.EndOfStream)
                break
            else
                return err;
        };

        repl.feed_line(line_buf.items) catch |err| {
            try stdout.print("> {}\n", .{err});
            continue;
        };

        try stdout.writeAll("> OK\n");
    }

    // read from stdin until exit
}

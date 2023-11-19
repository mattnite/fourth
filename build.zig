const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    _ = b.addModule("fourth", .{
        .source_file = .{
            .path = "src/fourth.zig",
        },
    });

    const repl = b.addExecutable(.{
        .name = "fourth-repl",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(repl);

    const main_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/fourth.zig" },
        .target = target,
        .optimize = optimize,
    });

    const run_main_tests = b.addRunArtifact(main_tests);
    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&run_main_tests.step);
}

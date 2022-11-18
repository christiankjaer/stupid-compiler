const std = @import("std");

const bool_f = 0x6;
const bool_t = 0x16;
const unit_t = 0xE;
const int_mask = 0x1;
const int_tag = 0x1;
const int_shift = 1;
const ch_mask = 0xF;
const ch_tag = 0xA;
const ch_shift = 4;

const stack_size = 4096;

extern fn program_entry(stack: [*c]u64, size: u64) u64;

pub fn main() anyerror!void {
    const stdout = std.io.getStdOut().writer();
    const allocator = std.heap.page_allocator;
    const stack = try allocator.alloc(u64, stack_size);
    defer allocator.free(stack);

    const res = program_entry(@ptrCast([*c]u64, stack), stack_size);

    if (res & int_mask == int_tag) {
        const shifted = @bitCast(i64, res) >> int_shift;
        try stdout.print("{d}\n", .{shifted});
    } else if (res & ch_mask == ch_tag) {
        // TODO: Nicer printing
        const shifted = @truncate(u8, res >> ch_shift);
        try stdout.print("{c}\n", .{shifted});
    } else if (res == bool_f) {
        _ = try stdout.write("false\n");
    } else if (res == bool_t) {
        _ = try stdout.write("true\n");
    } else if (res == unit_t) {
        _ = try stdout.write("()\n");
    } else {
        try stdout.print("#<unknown 0x{X}>", .{res});
    }
}

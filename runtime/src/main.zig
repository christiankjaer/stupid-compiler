const std = @import("std");

const bool_f = 0x2f;
const bool_t = 0x6f;
const unit_t = 0x3f;
const int_mask = 0x03;
const int_tag = 0x00;
const int_shift = 2;
const ch_mask = 0xFF;
const ch_tag = 0x0F;
const ch_shift = 8;

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
        try stdout.print("#\\{c}\n", .{shifted});
    } else if (res == bool_f) {
        _ = try stdout.write("#f\n");
    } else if (res == bool_t) {
        _ = try stdout.write("#t\n");
    } else if (res == unit_t) {
        _ = try stdout.write("()\n");
    } else {
        try stdout.print("#<unknown 0x{X}>", .{res});
    }
}

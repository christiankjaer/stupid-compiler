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

pub export fn print_const(res: u64) void {

    const stdout = std.io.getStdOut().writer();

    if (res & int_mask == int_tag) {
        const shifted = @bitCast(i64, res) >> int_shift;
        stdout.print("{d}\n", .{shifted}) catch {};
    } else if (res & ch_mask == ch_tag) {
        // TODO: Nicer printing
        const shifted = @truncate(u8, res >> ch_shift);
        stdout.print("{c}\n", .{shifted}) catch {};
    } else if (res == bool_f) {
        _ = stdout.write("false\n") catch 0;
    } else if (res == bool_t) {
        _ = stdout.write("true\n") catch 0;
    } else if (res == unit_t) {
        _ = stdout.write("()\n") catch 0;
    } else {
        stdout.print("#<unknown 0x{X}>", .{res}) catch {};
    }
}

pub fn main() anyerror!void {
    const allocator = std.heap.page_allocator;
    const stack = try allocator.alloc(u64, stack_size);
    defer allocator.free(stack);

    const res = program_entry(@ptrCast([*c]u64, stack), stack_size);

    print_const(res);

}

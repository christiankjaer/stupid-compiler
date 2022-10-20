const std = @import("std");
const opt = @import("build_options").entry;

comptime {
    asm (@embedFile(opt));
}

const bool_f = 0x1f;
const bool_t = 0x2f;
const null_t = 0x3f;
const fx_mask = 0x03;
const fx_tag = 0x00;
const fx_shift = 2;
const ch_mask = 0xFF;
const ch_tag = 0x0F;
const ch_shift = 8;

extern fn scheme_entry() u64;

pub fn main() anyerror!void {
    const stdout = std.io.getStdOut().writer();

    const res = scheme_entry();

    if (res & fx_mask == fx_tag) {
        const shifted = @bitCast(i64, res) >> fx_shift;
        try stdout.print("{d}\n", .{shifted});
    } else if (res & ch_mask == ch_tag) {
        // TODO: Nicer printing
        const shifted = @truncate(u8, res >> ch_shift);
        try stdout.print("#\\{c}\n", .{shifted});
    } else if (res == bool_f) {
        _ = try stdout.write("#f\n");
    } else if (res == bool_t) {
        _ = try stdout.write("#t\n");
    } else if (res == null_t) {
        _ = try stdout.write("()\n");
    } else {
        try stdout.print("#<unknown 0x{X}>", .{res});
    }
}

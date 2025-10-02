const std = @import("std");

// Emacs types
pub const EmacsValue = ?*anyopaque;
pub const EmacsFunction = *const fn (*EmacsEnv, isize, [*c]EmacsValue, ?*anyopaque) callconv(.C) EmacsValue;

// Emacs environment structure
pub const EmacsEnv = extern struct {
    size: usize,
    private_members: ?*anyopaque,

    // Core functions
    make_function: ?*const fn (*EmacsEnv, isize, isize, EmacsFunction, [*c]const u8, ?*anyopaque) callconv(.C) EmacsValue,
    funcall: ?*const fn (*EmacsEnv, EmacsValue, isize, [*c]EmacsValue) callconv(.C) EmacsValue,
    intern: ?*const fn (*EmacsEnv, [*c]const u8) callconv(.C) EmacsValue,

    // Type checking
    type_of: ?*const fn (*EmacsEnv, EmacsValue) callconv(.C) EmacsValue,
    is_not_nil: ?*const fn (*EmacsEnv, EmacsValue) callconv(.C) bool,
    eq: ?*const fn (*EmacsEnv, EmacsValue, EmacsValue) callconv(.C) bool,

    // Numbers
    extract_integer: ?*const fn (*EmacsEnv, EmacsValue) callconv(.C) i64,
    make_integer: ?*const fn (*EmacsEnv, i64) callconv(.C) EmacsValue,
    extract_float: ?*const fn (*EmacsEnv, EmacsValue) callconv(.C) f64,
    make_float: ?*const fn (*EmacsEnv, f64) callconv(.C) EmacsValue,

    // Strings
    copy_string_contents: ?*const fn (*EmacsEnv, EmacsValue, [*c]u8, *isize) callconv(.C) bool,
    make_string: ?*const fn (*EmacsEnv, [*c]const u8, isize) callconv(.C) EmacsValue,

    // User pointers
    make_user_ptr: ?*const fn (*EmacsEnv, ?*const fn (?*anyopaque) callconv(.C) void, ?*anyopaque) callconv(.C) EmacsValue,
    get_user_ptr: ?*const fn (*EmacsEnv, EmacsValue) callconv(.C) ?*anyopaque,
    set_user_ptr: ?*const fn (*EmacsEnv, EmacsValue, ?*anyopaque) callconv(.C) void,
    get_user_finalizer: ?*const fn (*EmacsEnv, EmacsValue) callconv(.C) ?*const fn (?*anyopaque) callconv(.C) void,
    set_user_finalizer: ?*const fn (*EmacsEnv, EmacsValue, ?*const fn (?*anyopaque) callconv(.C) void) callconv(.C) void,

    // Vectors
    vec_get: ?*const fn (*EmacsEnv, EmacsValue, isize) callconv(.C) EmacsValue,
    vec_set: ?*const fn (*EmacsEnv, EmacsValue, isize, EmacsValue) callconv(.C) void,
    vec_size: ?*const fn (*EmacsEnv, EmacsValue) callconv(.C) isize,
};

// Emacs runtime structure
pub const EmacsRuntime = extern struct {
    size: usize,
    private_members: ?*anyopaque,
    get_environment: ?*const fn (*EmacsRuntime) callconv(.C) ?*EmacsEnv,
};

// Helper functions for working with Emacs values
pub fn makeString(env: *EmacsEnv, str: []const u8) EmacsValue {
    return env.make_string.?(env, @ptrCast(str.ptr), @intCast(str.len));
}

pub fn extractString(env: *EmacsEnv, value: EmacsValue, allocator: std.mem.Allocator) ![]u8 {
    var size: isize = 0;
    _ = env.copy_string_contents.?(env, value, null, &size);

    const buffer = try allocator.alloc(u8, @intCast(size));
    _ = env.copy_string_contents.?(env, value, @ptrCast(buffer.ptr), &size);

    // Remove null terminator
    return buffer[0..@intCast(size - 1)];
}

pub fn makeNil(env: *EmacsEnv) EmacsValue {
    return env.intern.?(env, "nil");
}

pub fn makeT(env: *EmacsEnv) EmacsValue {
    return env.intern.?(env, "t");
}

pub fn makeList(env: *EmacsEnv, items: []const EmacsValue) EmacsValue {
    if (items.len == 0) {
        return makeNil(env);
    }

    const list_fn = env.intern.?(env, "list");
    var args = std.ArrayList(EmacsValue).init(std.heap.page_allocator);
    defer args.deinit();

    for (items) |item| {
        args.append(item) catch return makeNil(env);
    }

    return env.funcall.?(env, list_fn, @intCast(args.items.len), @ptrCast(args.items.ptr));
}

pub fn makeCons(env: *EmacsEnv, car: EmacsValue, cdr: EmacsValue) EmacsValue {
    const cons_fn = env.intern.?(env, "cons");
    var args = [_]EmacsValue{ car, cdr };
    return env.funcall.?(env, cons_fn, 2, &args);
}

// Function registration helper
pub fn registerFunction(
    env: *EmacsEnv,
    name: []const u8,
    min_arity: isize,
    max_arity: isize,
    function: EmacsFunction,
    documentation: []const u8,
) !void {
    const name_val = makeString(env, name);
    const doc_cstr = @as([*c]const u8, @ptrCast(documentation.ptr));

    const func_val = env.make_function.?(
        env,
        min_arity,
        max_arity,
        function,
        doc_cstr,
        null,
    );

    const defalias = env.intern.?(env, "defalias");
    var args = [_]EmacsValue{ name_val, func_val };
    _ = env.funcall.?(env, defalias, 2, &args);
}

// Error handling
pub fn signalError(env: *EmacsEnv, error_symbol: []const u8, data: []const u8) EmacsValue {
    const signal_fn = env.intern.?(env, "signal");
    const error_sym = env.intern.?(env, @ptrCast(error_symbol.ptr));
    const error_data = makeString(env, data);

    var args = [_]EmacsValue{ error_sym, error_data };
    return env.funcall.?(env, signal_fn, 2, &args);
}
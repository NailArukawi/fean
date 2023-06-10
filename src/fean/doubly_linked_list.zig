const std = @import("std");

pub fn DoublyLinkedList(comptime T: type) type {
    return struct {
        next: ?*@This(),
        prev: ?*@This(),
        data: T,

        pub fn remove(self: *@This()) void {
            var next = self.next.*;
            var prev = self.prev.*;

            prev.next = self.next.*;
            next.prev = self.prev.*;
        }

        pub fn append(self: *@This(), appendee: *@This()) void {
            self.next = appendee;
            appendee.prev = self;
        }
    };
}

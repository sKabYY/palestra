#!/usr/bin/env node

var Monad = function (v) {
    this.value = v;
    return this;
};

Monad.prototype.bind = function (f) {
    return f(this.value)
};

var lift = function (v) {
    return new Monad(v);
};

console.log(lift(32).bind(function (a) {
    return lift(a/2);
}));

var half = function (a) {
    return lift(a/2);
};

var print = function (a) {
    console.log(a);
    return lift(a);
};

console.log('---');

lift(32)
    .bind(half)
    .bind(print)
    .bind(half)
    .bind(print);

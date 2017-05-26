let sample0 = /([A-Z])\w+/g;
let sample1 = Rseq(
    Rpred(isUpperCase),
    Rplus(Rpred(isWhitespace))).compile();

let isUpperCase = function (c) { return c === 'A'; };
let isWhitespace = function (c) { return c === ' '; };

let Rseq = function () {
    let args = arguments;
    return {
        compile: function () {
            let regs = [];
            for (let i = 0; i < args.length; ++i) {
                regs.push(args[i].compile());
            }
            return function (s, start) {
                for (let i = 0; i < regs.length; ++i) {
                    let m = regs[i](s, start);
                    if (m) {
                        start = m.end;
                    } else {
                        return false;
                    }
                }
                return { result: true, end: start };
            };
        }
    };
};
let Ror = function () {
    let args = arguments;
    return {
        compile: function () {
            let regs = []
            for (let i = 0; i < args.length; ++i) {
                regs.push(args[i].compile());
            }
            return function (s, start) {
                ??
            };
        }
    };
};
let Rstar;
let Rpred;

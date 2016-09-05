#lang racket

(provide (all-defined-out))

(struct error:interp (message))
(struct error:return-outside-function error:interp ())
(struct error:unbound error:interp ())
(struct error:type-checking error:interp ())
(struct error:argsnum-not-match error:interp ())
(struct error:index-out-of-range error:interp ())
(struct error:mk-array-invalid-length error:interp ())

<TeXmacs|1.0.7.21>

<style|generic>

<\body>
  <\scm-code>
    target := (lambda (...) body[target])
  </scm-code>

  <\scm-code>
    p := (lambda (f)

    \ \ (lambda (...) body[x[f]]))
  </scm-code>

  <\scm-code>
    (p ?) = target and x[?] = target
  </scm-code>

  <\scm-code>
    (p ?) = x[?]
  </scm-code>

  <\scm-code>
    (lambda (...) body[x[?]]) = x[?]
  </scm-code>

  <\scm-code>
    K := (lambda (X) (lambda (...) body[X]))
  </scm-code>

  <\scm-code>
    (K x[?]) = x[?]
  </scm-code>

  <\scm-code>
    guess x[?] = (? ?) \ \ \ (K (? ?)) = (? ?) \ \ \ ? = lambda (f) (K (f f))
  </scm-code>

  <\scm-code>
    target

    = x[?]

    = (? ?)

    = ((lambda (f) (K (f f))) (lambda (f) (K (f f))))

    = ((lambda (g) ((lambda (f) (g (f f))) (lambda (f) (g (f f))))) K)
  </scm-code>

  <\scm-code>
    Y =

    (lambda (g)

    \ \ ((lambda (f) (g (f f)))

    \ \ \ (lambda (f) (g (f f)))))

    = \ ; also

    (lambda (g)

    \ \ ((lambda (f) (g (f f)))

    \ \ \ (lambda (f)

    \ \ \ \ \ (g

    \ \ \ \ \ \ lambda (...) ((f f) ...)))))
  </scm-code>
</body>
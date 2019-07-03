(load "mklib.ss")

(define love-rel (list
  (list 'zhangxueyou 'wanfei)
  (list 'zhangxueyou 'zouhuimin)
  (list 'wanfei 'xietinfen)
  (list 'zouhuimin 'zhangxueyou)
  (list 'xietinfen 'wanfei)
  (list 'xietinfen 'zouhuimin)
  (list 'liudehua 'zouhuimin)))

(define (love X Y)
  (membero (list Y X) love-rel))

(pretty-print
  (run* (x y)
    (love x y)
    (love y x)))

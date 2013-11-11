
#lang racket

;; Você deve implementar as funções neste arquivo. Novas funções podem ser
;; criadas, mas todas as funções devem ter testes (no arquivo testes.rkt).
;;
;; Observe que algumas destas funções não tem testes, faz parte do trabalho
;; criar estes testes.
;;
;; Você não precisa se preocupar com ler a tecla pressionada ou desenhar o jogo
;; na tela. O arquivo main.rkt chama uma função que faz isso. Basta você
;; implementar as funções deste arquivo que o jogo funciona.
;;
;; Para ter uma ideia do processo de execução do jogo, execute o arquivo
;; main.rkt sem mudar nada neste arquivo. Uma janela irá aparecer. Pressione
;; algumas teclas e observe a saída no console do DrRacket. Veja o corpo
;; inicial das funções make-tetris-padrao, trata-tecla, trata-tick e desenha.

(require "base.rkt")
(require 2htdp/image)
(require 2htdp/universe)

(provide make-tetris-padrao
         tetramino->lista-pos
         lop-validas?
         lop-livres?
         fixa
         limpa
         trata-tecla
         trata-tick
         desenha)

;; -> Tetris
;; Cria o jogo inicial.
;; Esta função é chamada no arquivo main.rkt.
(define (make-tetris-padrao)
  (make-tetris LARGURA-PADRAO ALTURA-PADRAO (stream-tetraminos) TIMEOUT-PADRAO 1 0))

;; Jogo String -> Jogo
;; Esta função é chamada quando uma tecla é pressionada.
;; Devolve um jogo com o tetraminó que está caindo movido de acordo com a tecla
;;   "right" - tenta mover para direita
;;   "left"  - tenta mover para esquerda
;;   "up"    - tenta rotacionar
;;   "down"  - tenta mover para baixo
;;
;; Se a tecla for "right", "left" ou "up" e o movimento não puder ser
;; realizado, o jogo é devolvido sem modificações.
;;
;; Se a tecla for "down" e o movimento não puder ser realizado, tetra é fixado
;; no campo, as linhas completas são removidas, o próximo tetraminó é
;; selecionada para cair e o contador de automovimento retorna ao valor
;; inicial.
;;
;; Se o movimento puder ser realizado, o jogo após o movimento é devolvido.
;;
;; Use a função key=? para comparar o tecla com os valores "right", "left, "up"
;; e "down".

(define (novotetra jogo)
  (define nov-tetra (stream-first (tetris-proximos jogo)))
  (struct-copy tetris jogo (tetra nov-tetra)))

(define (novastream jogo)
  (define nov-stream (stream-tetraminos))
  (if (lop-livres? (tetramino->lista-pos (tetris-tetra jogo)) (tetris-campo jogo))
      (struct-copy tetris jogo (proximos nov-stream))
      (printf "perdeu!")))

(define (fixa-tetra jogo)
  (novastream (novotetra jogo)))

(define (move jogo n)
  (define (down tetra)
        (struct-copy tetramino tetra (pos (posn (posn-lin (tetramino-pos tetra)) (+ n (posn-col (tetramino-pos tetra)))))))
  (struct-copy tetris jogo (tetra (down (tetris-tetra jogo)))))

(define (move-down jogo)
  (define (down tetra)
        (struct-copy tetramino tetra (pos (posn (add1 (posn-lin (tetramino-pos tetra))) (posn-col (tetramino-pos tetra))))))
  (struct-copy tetris jogo (tetra (down (tetris-tetra jogo)))))
  
(define (rotat jogo)
  (define (rotat-t tetra)
    (if (equal? (tetramino-rot tetra) (sub1 (length (tetramino-tipo tetra))))
        (struct-copy tetramino tetra (rot 0))
        (struct-copy tetramino tetra (rot (add1 (tetramino-rot tetra))))))
  (struct-copy tetris jogo (tetra (rotat-t (tetris-tetra jogo)))))

(define (desce jogo)
  (if (lop-livres? (tetramino->lista-pos (tetris-tetra (move-down jogo))) (tetris-campo jogo))
                       (desce (move-down jogo))
                       (fixa-tetra (fixa jogo))))

(define (trata-tecla jogo tecla)

  (cond
    [(key=? tecla "right") 
     (if (lop-livres? (tetramino->lista-pos (tetris-tetra (move jogo 1))) (tetris-campo jogo))
         (move jogo 1) jogo)]
    [(key=? tecla "left")
     (if (lop-livres? (tetramino->lista-pos (tetris-tetra (move jogo -1))) (tetris-campo jogo))
         (move jogo -1) jogo)]
    [(key=? tecla "up") 
     (if (lop-livres? (tetramino->lista-pos (tetris-tetra (rotat jogo))) (tetris-campo jogo))
         (rotat jogo) jogo)]
    [(key=? tecla "down")
     (if (lop-livres? (tetramino->lista-pos (tetris-tetra (move-down jogo))) (tetris-campo jogo))
                       (move-down jogo)
                       (fixa-tetra (fixa jogo)))]
    [(key=? tecla " ") (desce jogo)]
    [else jogo]))

;; Jogo -> Jogo
;; Função que trata um tick. Esta função é chamada 28 vezes por segundo, ela
;; deve mover o tetra para baixo depois que uma determinada quantidade
;; (TIMEOUT-PADRAO) de ticks. Se o jogador mover para baixo e fixar o
;; tetraminó, a contagem deve reiniciar.

(define (verif-time jogo)
    (define zera (struct-copy tetris jogo (timeout 0)))
  (if (> (tetris-timeout jogo) TIMEOUT-PADRAO) (if (lop-livres? (tetramino->lista-pos (tetris-tetra (move-down jogo))) (tetris-campo jogo))
                       (move-down zera)
                       (fixa-tetra (fixa zera)))
      (struct-copy tetris jogo (timeout (+ (tetris-timeout jogo) (tetris-nivel jogo))))))

(define (trata-tick jogo)
  (define zera (struct-copy tetris jogo (pt 0)))
  (define evolui (if (> (tetris-pt jogo) 2000) (struct-copy tetris zera (nivel (add1 (tetris-nivel jogo)))) jogo))
  (define pontua (struct-copy tetris evolui (pt (+ (tetris-nivel jogo) (tetris-pt evolui)))))
  (limpa (verif-time pontua)))

;; Tetris -> Imagem
;; Esta função é chamada quando o jogo precisa ser desenhado na tela. Devolve
;; uma imagem que representa o jogo.
;; Veja as funções pré-definidas rectangle, beside, above e overlay no pacote
;; 2htdp/image.

(define (desenha-camp campo)
  (define (desenha-l lst)
    (foldl (λ (p a) (beside a (if (equal? "black" (list-ref CORES p))
                     (rectangle Q-LARGURA Q-ALTURA "solid" "black")
                     (overlay (overlay (rectangle (- Q-LARGURA 6) (- Q-ALTURA 6) "solid" (list-ref CORES p)) (rectangle (- Q-LARGURA 1) (- Q-ALTURA 1) "solid" "gray"))
                              (rectangle Q-LARGURA Q-ALTURA "solid" "black")))))
                  BLANK
                  lst))
  (foldl (λ (l rst) (above rst (desenha-l l))) BLANK campo))

(define (desenha jogo)
  (overlay
   (beside (desenha-camp (tetris-campo (fixa jogo)))
           (above (overlay (desenha-camp (first (tetramino-tipo (stream-first (tetris-proximos jogo)))))
                     (beside (rectangle 3 110 "solid" "gray") (rectangle (* 4 Q-LARGURA) (* 4 Q-ALTURA) "solid" "black")))
                   (beside (rectangle 3 350 "solid" "gray") (above 
                                                             (above (text "Nível" 20 "white") (text (number->string (tetris-nivel jogo)) 20 "white"))
                                                             (above (text "Pontuação" 20 "white") (text (number->string (tetris-pt jogo)) 20 "white"))))))
   (rectangle 430 610 "solid" "gray")))

;; Tetramino -> Lista(Posn)
;; Devolve a lista de posições que t ocupa no campo considerando a rotação e a
;; posição (translação em relação a origem).
;; 
;; Por exemplo, seja TT1 definido como
;; (define TT1 (tetramino T_TIPOS 1 (posn 1 0) T_COR))
;; este tetraminó está na rotação 1 e na posição (posn 1 0). O elemento na
;; posição 1 de T_TIPOS é T1 que é a seguinte lista de listas (definina em
;; tetra-tipos.rkt)
;;    0 1 2     ; colunas
;;              ; linhas
;; '((0 1 0)    ; 0
;;   (0 1 1)    ; 1
;;   (0 1 0)))  ; 2
;;
;; As as posições ocupadas por T1 são marcadas com 1, ou seja, as posições
;; ocupadas por T1 são (posn 0 1) (posn 1 1) (posn 1 2) e (posn 2 1). Estas São
;; as posições em relação a (posn 0 0), mas o tetraminó está na posição
;; (posn 1 0), desta forma, precisamos fazer a translação das posições. Para
;; isto, somamos o ponto (posn 1 0) a cada ponto de T1, o que resulta em
;; (pos 1 1) (posn 2 0) (posn 2 2) (posn 3 1). Observe que é posível ter
;; um deslocamento em relação a origem negativa. Por exemplo, se a posição de
;; TT1 fosse (posn 0 -1), obteríamos como resposta da função a lista com
;; as posições (posn 0 0) (posn 1 0) (pos 1 1) (pos 2 0).
;;
;; Veja os testes para outros exemplos de como esta função deve funcionar.

(define (lista-linha lst l c desl)
  (cond
    [(empty? lst) empty]
    [(not(= (first lst) 0)) (cons (posn (+ l (posn-lin desl)) (+ c (posn-col desl))) (lista-linha (rest lst) l (add1 c) desl))]
    [else (lista-linha (rest lst) l (add1 c) desl)]))

(define (lista-tetra lst l desl)
  (cond
    [(empty? lst) empty]
    [else (append (lista-linha (first lst) l 0 desl) (lista-tetra (rest lst) (add1 l) desl))]))
  
(define (tetra-lista-pos tetra n)
  (cond 
    [(equal? n 0) (first tetra)]
    [else (tetra-lista-pos (rest tetra) (sub1 n))]))

(define (tetramino->lista-pos t)
  (lista-tetra (tetra-lista-pos (tetramino-tipo t) (tetramino-rot t)) 0 (tetramino-pos t)))

;; Lista(Posn) Natural Natural -> Boolean
;; Devolve verdadeiro se todas os posições de lp são válidas, isto é, estão
;; dentro de um campo de tamanho largura x altura. Devolve falso caso
;; contrário.

(define (lop-validas? lp largura altura)
  (define (posn-valida? p)
    (and (<= 0 (posn-lin p)) (<= 0 (posn-col p))
       (< (posn-lin p) altura) (< (posn-col p) largura)))
  (andmap posn-valida? lp))

;; Lista(Posn) Campo -> Boolean
;; Devolve verdadeiro se todas as posição de lp estão livres no campo. Devolve
;; falso caso contrário.
;; Requer que todas as posições em lp sejam válidas.

(define (lop-livres? lp campo)
  (if (lop-validas? lp (length (first campo)) (length campo))
      (andmap (λ (p) (equal? 0 (list-ref (list-ref campo (posn-lin p)) (posn-col p)))) lp)
      #f))

;; Jogo -> Jogo
;; Preenche as posições ocupadas pelo tetraminó (que está caindo) no campo do
;; jogo.
;; Requer que tetraminó não possa ser movido para baixo.

(define (fixa jogo)
  (define (set-p lst c cor)
    (append (take lst c) (list cor) (drop lst (add1 c))))
  (define (set-posn posn lst)
    (append (take lst (posn-lin posn)) 
            (list (set-p (first (drop lst (posn-lin posn))) 
                         (posn-col posn) 
                         (tetramino-cor (tetris-tetra jogo))))
            (drop lst (add1 (posn-lin posn)))))
  (define (fixa-tetra campo)
    (foldr set-posn (tetris-campo jogo) (tetramino->lista-pos (tetris-tetra jogo))))
  (struct-copy tetris jogo (campo (fixa-tetra (tetris-campo jogo)))))
  
;; Jogo -> Jogo
;; Devolve um jogo sem as linhas que estão completas, isto é, as linhas que não
;; tem nenhum quadrado vazio. O jogo devolvido tem o mesmo tamanho do jogo de
;; entrada.

(define (linha-full? lst)
  (andmap (λ (x)(> x 0)) lst))

(define (remove-full lst)
  (define (remove lst rst)
    (cond
      [(linha-full? lst) rst]
      [else (cons lst rst)]))
  (foldr remove empty lst))

(define (completa lst alt larg)
  (define (add-lin lst n)
    (cond
      [(equal? n 0) lst]
      [else (cons (make-linha larg) (add-lin lst (sub1 n)))]))
  (add-lin lst (- alt (length lst))))

(define (limpa jogo)
  (struct-copy tetris jogo (campo (completa 
                                   (remove-full (tetris-campo jogo))
                                   (length (tetris-campo jogo))
                                   (length (first (tetris-campo jogo)))))))

;; -> Stream(Tetramino)
;; Cria um stream randômico de tetraminós.
;; Esta função não precisa de testes.
;; Você tem que implementar esta função, o corpo incial deve ser descartado.
(define (stream-tetraminos)
  (define a (random 7))
  (stream-cons (centraliza (list-ref TETRAMINOS a) LARGURA-PADRAO) (stream-tetraminos)))

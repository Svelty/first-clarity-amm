;; title: constant-product-amm2
;; version:
;; summary:
;; description:

;; rethinking first attempt was too complicated, focus on writing a contract for exchaing two known assets


;; requirements:
;; whitelist tokens for listing
;; list a pair of tokens
;; enable trading for pair
;; add liquidity to pair (exchange tokens for LP token)
;; exchange token for given pair
;; get existing pairs
;; remove liquidity (exchange LP token for tokens)
;; disable trading for pair

;; https://solidity-by-example.org/defi/constant-product-amm/ - example


;;problems: first itteration too generalised, start with case of swaping single pair


;; traits
(use-trait ft-trait 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.sip-010-trait-ft-standard.sip-010-trait)

;; token definitions

;; constants
(define-constant deployer tx-sender)

;;errors
(define-constant err-not-sip-010-token (err u1000))
(define-constant err-token-not-whitelisted (err u1001))
(define-constant err-not-owner (err u1003))
(define-constant err-token-not-registered (err u1004))
(define-constant err-token-already-registerd (err u1005))
(define-constant err-pair-already-exists (err u1006))
(define-constant err-invalid-id (err u1007))
(define-constant err-invalid-token (err u1008))
(define-constant err-add-liquidity-invalid-amount (err u1009))
(define-constant err-token-balance-zero (err u1010))

;; data vars
;; is there any reason to use ids or will tokens always be identified by principal?
(define-data-var owner principal tx-sender)
(define-data-var token-nonce uint u0)
(define-data-var pair-nonce uint u0)

(define-data-var token1 principal .test-coin1)
(define-data-var token2 principal .test-coin2)
(define-data-var lp-token principal .cpm2-lp-token)




;; xy = k
;; (x + dx)(y + dy) = k'

;; to add liquidity without chaning price ratio must stay constant 
;; x / y = (x + dx) / (y + dy)
;; x(y + dy) = y(x +dx)
;; xy + xdy = yx + ydx
;; xdy = ydx
;; dy = ydx/x     to add amount dx you must add amount dy equal to current amount y times amount to add dx divided by current amount x

(define-public (add-liquidity (token1-contract <ft-trait>) (token1-amount uint) (token2-contract <ft-trait>) (token2-max-amount uint) (lp-token-contract <ft-trait>))
    ;; need to add assertions to make sure token1-contract = test-coin1 and token2-contract = test-coin2 and lp-toke-contract = lp-token
    (let
        (
            (this-contract (as-contract tx-sender))
            (token1-balance (try! (contract-call? token1-contract get-balance this-contract)))
            (token2-balance (try! (contract-call? token2-contract get-balance this-contract)))
            (token2-required-amount (calculate-add-liquidity-required-amount token1-amount token1-balance token2-balance))
            (lp-token-supply (try! (contract-call? lp-token-contract get-total-supply)))
            (sender tx-sender)
            (token2-amount (if (is-eq token2-required-amount u0) token2-max-amount token2-required-amount))
        )
        (asserts! (>= token2-max-amount token2-amount) err-add-liquidity-invalid-amount)
        (try! (contract-call? token1-contract transfer token1-amount tx-sender this-contract none))
        (try! (contract-call? token2-contract transfer token2-amount tx-sender this-contract none))
        (if (is-eq lp-token-supply u0)
            (try! (as-contract (contract-call? .cpm2-lp-token mint (sqrti (* token1-amount token2-amount)) sender)))
            ;; needs some checks to make sure we arn't dividing by 0
            (if (< (/ (* token1-amount lp-token-supply) token1-balance) (/ (* token2-amount lp-token-supply) token2-balance))
                (try! (as-contract (contract-call? .cpm2-lp-token mint (/ (* token1-amount lp-token-supply) token1-balance) sender)))
                (try! (as-contract (contract-call? .cpm2-lp-token mint (/ (* token2-amount lp-token-supply) token2-balance) sender)))
            )
        )
        (ok true)

        ;; if lp token total supply = 0 mint(token1-amount * token2-max-amount)
        ;; else 


        ;; TODO: liquidity tokens
    )
)

;; pricing - constant product x * y = k (token_suppy_y * token_supply_x = constant_k)
;; (x + dx)(y - dy) = k   for a change in y, change in x must corospond to keep k constant 
;; y - dy = k/(x+dx)
;; y - k/(x+dx) = dy
;; dy = y - k/(x+dx)
;; dy = (x+dx)y/(x+dx) - k/(x+dx)
;; dy = (xy + dxy -k)/(x+dx)
;; dy = (k + dxy - k)/(x+dx)
;; dy = dxy/(x + dx)     for a change of x dx, change of y dy is equal to the change of x times the supply of y divided by the supply of x plus the change of x

;; price of x = token_supply_y/token_supply_x
;; price of y = token_supply_x/token_supply_y


(define-public (swap (from-token-contract <ft-trait>) (from-amount uint) (to-token-contract <ft-trait>))
    (let
        (
            (this-contract (as-contract tx-sender))
            (from-token-balance (try! (contract-call? from-token-contract get-balance this-contract)))
            (to-token-balance (try! (contract-call? to-token-contract get-balance this-contract)))
            (to-amount (calculate-swap-amount from-amount from-token-balance to-token-balance))
            (sender tx-sender)
        )
        (asserts! (or 
            (and (is-eq (contract-of from-token-contract) (var-get token1)) (is-eq (contract-of to-token-contract) (var-get token2)))
            (and (is-eq (contract-of from-token-contract) (var-get token2)) (is-eq (contract-of to-token-contract) (var-get token1)))
        ) err-invalid-token)
        (asserts! (not (is-eq to-amount u0)) err-token-balance-zero)
        (try! (contract-call? from-token-contract transfer from-amount tx-sender this-contract none))
        (try! (as-contract (contract-call? to-token-contract transfer to-amount this-contract sender none)))
        (ok true)
    )
)



;; read only functions
(define-read-only (calculate-add-liquidity-required-amount (amount1 uint) (balance1 uint) (balance2 uint))
    (if (or (is-eq balance1 u0) (is-eq balance2 u0))
        u0
        (/ (* amount1 balance2) balance1)
    )
)

(define-read-only (calculate-swap-amount (amount1 uint) (balance1 uint) (balance2 uint))
    (if (or (is-eq balance1 u0) (is-eq balance2 u0))
        u0
        (/ (* amount1 balance2) (+ amount1 balance1))
    )
)

;; (define-read-only (add-liquidity-amount-required (pair-id uint) (token-id uint) (token-amount uint))
;;     (begin 
;;         (let 
;;             (
;;                 (pair (unwrap! (map-get? pairs pair-id) err-invalid-id))
;;                 (token1-id (get token1 pair))
;;                 (token2-id (get token2 pair))
;;                 (token1-balance (get token1-balance pair))
;;                 (token2-balance (get token2-balance pair))
;;             )
;;             ;; if either balance is 0, any amount can be added to set the initial price
;;             (if (or (is-eq token1-balance u0) (is-eq token2-balance u0))
;;                 (ok u0)
;;                 (if (is-eq token-id token1-id)
;;                     (ok (/ (* token-amount token2-balance) token1-balance))
;;                     (if (is-eq token-id token2-id)
;;                         (ok (/ (* token-amount token1-balance) token2-balance))
;;                         err-token-not-in-pair
;;                     )
;;                 )
;;             )
            
;;         )
;;     )
;; )

;; private functions
(define-private (is-owner) 
    (ok (asserts! (is-eq tx-sender (var-get owner)) err-not-owner))
)


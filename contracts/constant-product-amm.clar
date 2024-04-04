;; title: constant-product-amm
;; version:
;; summary:
;; description:

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

;;to generalize lp token must store a map of token-ids and balances, transfer function must update maps
;;see "semi fungable token sip013"


;; traits
(use-trait ft-trait 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.sip-010-trait-ft-standard.sip-010-trait)
(use-trait sft-trait 'SPDBEG5X8XD50SPM1JJH0E5CTXGDV5NJTKAKKR5V.sip013-semi-fungible-token-trait.sip013-semi-fungible-token-trait)

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
(define-constant err-token-not-in-pair (err u1008))
(define-constant err-add-liquidity-invalid-amount (err u1009))

;; data vars
;; is there any reason to use ids or will tokens always be identified by principal?
(define-data-var owner principal tx-sender)

(define-data-var pair-nonce uint u0)
;; should this be a var or a const?
(define-data-var lp-token-principal principal .cpm-lp-token)



;; data maps

;; when checking pairs must check both directions, ie token1-token2 is the same pair as token2-token1
;; do i need to set decimals for pairs
(define-map pairs uint { token1: principal, token2: principal, is-open: bool, token1-balance: uint, token2-balance: uint, lp-token-id: uint })
(define-map pair-ids { token1: principal, token2: principal } uint)

;; public functions
(define-public (register-pair (token1 principal) (token2 principal))
    (begin 
        (let 
            (
                (pair-id (var-get pair-nonce))
                (lp-token-id (try! (as-contract (contract-call? .cpm-lp-token register-new-token pair-id token1 token2))))
            )
            (asserts! (not (does-pair-exist token1 token2)) err-pair-already-exists)
            (map-set pairs pair-id { token1: token1, token2: token2, is-open: false, token1-balance: u0, token2-balance: u0, lp-token-id: lp-token-id })
            (map-set pair-ids { token1: token1, token2: token2 } pair-id)
            (var-set pair-nonce (+ pair-id u1))
            (ok pair-id)
        )
    )
)

;; xy = k
;; (x + dx)(y + dy) = k'

;; to add liquidity without chaning price ratio must stay constant 
;; x / y = (x + dx) / (y + dy)
;; x(y + dy) = y(x +dx)
;; xy + xdy = yx + ydx
;; xdy = ydx
;; dy = ydx/x     to add amount dx you must add amount dy equal to current amount y times amount to add dx divided by current amount x

(define-public (add-liquidity (pair-id uint) (token1-contract <ft-trait>) (token1-amount uint) (token2-contract <ft-trait>) (token2-max-amount uint) (lp-token-contract <sft-trait>))
    (begin ;;do i actually need to use begin here?
        (let
            (
                (this-contract (as-contract tx-sender))
                (sender tx-sender)
                (pair (unwrap! (map-get? pairs pair-id) err-invalid-id))
                (token1-id (get token1 pair))
                (token2-id (get token2 pair))
                (token1-balance (get token1-balance pair));;check these for 0 balance
                (token2-balance (get token2-balance pair))
                (token2-required-amount (calculate-add-liquidity-required-amount token1-amount token1-balance token2-balance))
                (lp-token-id (get lp-token-id pair))
                (lp-token-supply (try! (contract-call? lp-token-contract get-total-supply lp-token-id)))
                (token2-amount (if (is-eq token2-required-amount u0) token2-max-amount token2-required-amount))
            )
            (asserts! (>= token2-max-amount token2-amount) err-add-liquidity-invalid-amount)
            (try! (contract-call? token1-contract transfer token1-amount tx-sender this-contract none))
            (try! (contract-call? token2-contract transfer token2-amount tx-sender this-contract none))
            (if (is-eq lp-token-supply u0)
                (try! (as-contract (contract-call? .cpm-lp-token mint (sqrti (* token1-amount token2-amount)) lp-token-id  sender)))
                ;; needs some checks to make sure we arn't dividing by 0
                (if (< (/ (* token1-amount lp-token-supply) token1-balance) (/ (* token2-amount lp-token-supply) token2-balance))
                    (try! (as-contract (contract-call? .cpm-lp-token mint (/ (* token1-amount lp-token-supply) token1-balance) lp-token-id sender)))
                    (try! (as-contract (contract-call? .cpm-lp-token mint (/ (* token2-amount lp-token-supply) token2-balance) lp-token-id sender)))
                )
            )
            (ok true)
        )
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

;; TODO: get add liquidity working first
;; (define-public (swap-deliver-exact (pair-id uint) (token-id uint) (amount uint))
;;     (begin
;;         (let 
;;             (
;;                 (pair (unwrap! (map-get? pairs pair-id) err-invalid-id))
;;                 (token1-id (get token1 pair))
;;                 (token2-id (get token2 pair))
;;                 (token1-balance (get token1-balance pair))
;;                 (token2-balance (get token2-balance pair))
;;                 (token1 (unwrap! (map-get? tokens token1-id) err-token-not-in-pair))
;;                 (token2 (unwrap! (map-get? tokens token2-id) err-token-not-in-pair))
;;                 (token1-contract (get contract token1))
;;                 (token2-contract (get contract token2))
;;             )
;;             ;; do i need to pass in the token traits?
;;             (ft-transfer? token1-contract amount tx-sender (as-contract tx-sender))
;;         )
;;     )
;; )



;; read only functions


(define-read-only (calculate-add-liquidity-required-amount (amount1 uint) (balance1 uint) (balance2 uint))
    (if (or (is-eq balance1 u0) (is-eq balance2 u0))
        u0
        (/ (* amount1 balance2) balance1)
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

(define-private (does-pair-exist (contract1 principal) (contract2 principal))
    (or (is-some (map-get? pair-ids { token1: contract1, token2: contract2 }))
        (is-some (map-get? pair-ids { token1: contract2, token2: contract1 }))
    )
)



;; (define-private (is-token-id-valid (token-id uint))
;;     (is-some (map-get? tokens token-id))
;; )

;; (define-private (is-whitelisted (token principal))
;;     (ok (asserts! (default-to false (map-get? whitelist token)) err-token-not-whitelisted))
;; )

;; (define-read-only (get-token-id (token principal))
;;     (map-get? token-ids token)
;; )

;; (define-public (add-token-to-whitelist (token <ft-trait>)) 
;;     (begin 
;;         (try! (is-owner))
;;         (ok (map-set whitelist (contract-of token) true))
;;     )
;; )

;; (define-public (remove-token-from-whitelist (token <ft-trait>)) 
;;     (begin 
;;         (try! (is-owner))
;;         (ok (map-set whitelist (contract-of token) false))
;;     )
;; )
;; (define-data-var token-nonce uint u0)
;; what other info about a token is needed: name, contract address, listedAt
;; (define-map whitelist principal bool)
;; is there any point in having tokens seperated from whitelist?
;; by having the token map use an int as a key you can iterate over the list of tokens - how do we stop a token from being listed more than once?
;; (define-map tokens uint { contract: principal, name: (string-ascii 32), ticker: (string-ascii 32), decimals: uint })
;; (define-map token-ids principal uint)
;; is there any reason to have this seperated from add to whitelist?
;; TODO: name, ticker and deciamls should come from the token contract get-name, get-symbol, get-decimals
;; (define-public (register-token (token <ft-trait>))
;;     (begin 
;;         (let 
;;             (
;;                 (token-id (var-get token-nonce))
;;                 (token-contract  (contract-of token))
;;                 (name (try! (contract-call? token get-name)))
;;                 (ticker (try! (contract-call? token get-symbol)))
;;                 (decimals (try! (contract-call? token get-decimals)))
;;             )
;;             (asserts! (is-none (get-token-id token-contract)) err-token-already-registerd)
;;             (try! (is-whitelisted token-contract))
;;             (map-set tokens token-id { contract: token-contract, name: name, ticker: ticker, decimals: decimals })
;;             (map-set token-ids token-contract token-id)
;;             (var-set token-nonce (+ token-id u1))
;;             (ok token-id)
;;         )
;;     )
;; )
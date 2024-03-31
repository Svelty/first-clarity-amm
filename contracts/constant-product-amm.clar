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
(define-constant err-token-not-in-pair (err u1008))
(define-constant err-add-liquidity-invalid-amount (err u1009))

;; data vars
;; is there any reason to use ids or will tokens always be identified by principal?
(define-data-var owner principal tx-sender)
(define-data-var token-nonce uint u0)
(define-data-var pair-nonce uint u0)


;; data maps
;; TODO: need to add sip010 trait - exchangable toeksn must be sip010
;; what other info about a token is needed: name, contract address, listedAt
;; TODO: whitelisted tokens must be sip010, do i add that here or do i handle that in the add to whitelist method?
(define-map whitelist principal bool)
;; is there any point in having tokens seperated from whitelist?
;; by having the token map use an int as a key you can iterate over the list of tokens - how do we stop a token from being listed more than once?
(define-map tokens uint { contract: principal, name: (string-ascii 32), ticker: (string-ascii 32), decimals: uint })
(define-map token-ids principal uint)
;; when checking pairs must check both directions, ie token1-token2 is the same pair as token2-token1
;; do i need to set decimals for pairs
(define-map pairs uint { token1: uint, token2: uint, is-started: bool, token1-balance: uint, token2-balance: uint })
(define-map pair-ids { token1: principal, token2: principal } uint)

;; public functions
(define-public (add-token-to-whitelist (token <ft-trait>)) 
    (begin 
        (try! (is-owner))
        (ok (map-set whitelist (contract-of token) true))
    )
)

(define-public (remove-token-from-whitelist (token <ft-trait>)) 
    (begin 
        (try! (is-owner))
        (ok (map-set whitelist (contract-of token) false))
    )
)

;; is there any reason to have this seperated from add to whitelist?
;; TODO: name, ticker and deciamls should come from the token contract get-name, get-symbol, get-decimals
(define-public (register-token (token <ft-trait>))
    (begin 
        (let 
            (
                (token-id (var-get token-nonce))
                (token-contract  (contract-of token))
                (name (try! (contract-call? token get-name)))
                (ticker (try! (contract-call? token get-symbol)))
                (decimals (try! (contract-call? token get-decimals)))
            )
            (asserts! (is-none (get-token-id token-contract)) err-token-already-registerd)
            (try! (is-whitelisted token-contract))
            (map-set tokens token-id { contract: token-contract, name: name, ticker: ticker, decimals: decimals })
            (map-set token-ids token-contract token-id)
            (var-set token-nonce (+ token-id u1))
            (ok token-id)
        )
    )
)

(define-public (register-pair (token1 uint) (token2 uint))
    (begin 
        (let 
            (
                (contract1 (unwrap! (get contract (map-get? tokens token1)) err-token-not-registered))
                (contract2 (unwrap! (get contract (map-get? tokens token2)) err-token-not-registered))
                (pair-id (var-get pair-nonce))
            )
            (asserts! (not (does-pair-exist contract1 contract2)) err-pair-already-exists)
            (map-set pairs pair-id { token1: token1, token2: token2, is-started: false, token1-balance: u0, token2-balance: u0})
            (map-set pair-ids { token1: contract1, token2: contract2 } pair-id)
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

(define-public (add-liquidity (pair-id uint) (token1-amount uint) (token2-amount uint))
    (begin 
        (let
            (
                (pair (unwrap! (map-get? pairs pair-id) err-invalid-id))
                (token1-id (get token1 pair))
                (token2-id (get token2 pair))
                (token1-balance (get token1-balance pair))
                (token2-balance (get token2-balance pair))
                (token2-required-amount (try! (add-liquidity-amount-required pair-id token1-id token1-amount)))
            )
            (if (not (is-eq token2-required-amount u0))
                (begin 
                    (asserts! (>= token2-amount token2-required-amount) err-add-liquidity-invalid-amount)
                    (map-set pairs pair-id (merge pair { token1-balance: (+ token1-balance token1-amount), token2-balance: (+ token2-balance token2-required-amount)}))
                    (ok true)
                )
                (begin
                    (map-set pairs pair-id (merge pair { token1-balance: (+ token1-balance token1-amount), token2-balance: (+ token2-balance token2-amount)}))
                    (ok true)
                )
            )
            ;; TODO: liquidity tokens
        )
    )
)



;; read only functions
(define-read-only (get-token-id (token principal))
    (map-get? token-ids token)
)

(define-read-only (add-liquidity-amount-required (pair-id uint) (token-id uint) (token-amount uint))
    (begin 
        (let 
            (
                (pair (unwrap! (map-get? pairs pair-id) err-invalid-id))
                (token1-id (get token1 pair))
                (token2-id (get token2 pair))
                (token1-balance (get token1-balance pair))
                (token2-balance (get token2-balance pair))
            )
            ;; if either balance is 0, any amount can be added to set the initial price
            (if (or (is-eq token1-balance u0) (is-eq token2-balance u0))
                (ok u0)
                (if (is-eq token-id token1-id)
                    (ok (/ (* token-amount token2-balance) token1-balance))
                    (if (is-eq token-id token2-id)
                        (ok (/ (* token-amount token1-balance) token2-balance))
                        err-token-not-in-pair
                    )
                )
            )
            
        )
    )
)

;; private functions
(define-private (is-owner) 
    (ok (asserts! (is-eq tx-sender (var-get owner)) err-not-owner))
)

(define-private (is-whitelisted (token principal))
    (ok (asserts! (default-to false (map-get? whitelist token)) err-token-not-whitelisted))
)

(define-private (is-token-id-valid (token-id uint))
    (is-some (map-get? tokens token-id))
)

(define-private (does-pair-exist (contract1 principal) (contract2 principal))
    (or (is-some (map-get? pair-ids { token1: contract1, token2: contract2 }))
        (is-some (map-get? pair-ids { token1: contract2, token2: contract1 }))
    )
)
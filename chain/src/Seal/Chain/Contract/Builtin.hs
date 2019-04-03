{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

module Seal.Chain.Contract.Builtin where

import Text.Trifecta as TF hiding (line,err,try,newline)
import Universum
import Data.String.QQ
import Seal.Contract.Types.Runtime
import Seal.Contract.Parse (exprsOnly)

-- 内置的token合约
tokenSrc :: String
tokenSrc = [s|

(env-data {"admin-keyset" {"keys" ["SEALuat7yPX4hQ2t4HN3UurJmUoHuqXbzWuDnokDgTt7YrwtaYgFy4G4qwuZMsW"]}})
(env-keys ["SEALuat7yPX4hQ2t4HN3UurJmUoHuqXbzWuDnokDgTt7YrwtaYgFy4G4qwuZMsW"])
(define-keyset 'admin-keyset (read-keyset "admin-keyset"))
(ns Token 'admin-keyset)

  ;; ==== real estate table ====
  (defrecord tokens
      [^string token
       ^string type
       ^string info
       ^string sendable
       ^string tx-id
       ^integer price
       ^integer total-shares
       ^integer available-shares])

  (deftable tokens-table ^{tokens}) ;; key: ^string token-id

  ;; ==== stake holder table ====
  (defrecord stakeholder
      [^address token-id
       ^address holder
       ^string is-owner
       ^string is-revoker
       ^integer shares  ;; or credits
       ])

  (deftable stakeholder-table ^{stakeholder}) ;; key: ^string (hash token-id holder)

  ;; ==== revenue table ====
  (defrecord revenue
    [
    ^address token-id
    ^address from
    ^string tx-id
    ^integer total-shares
    ^integer total-bonus
    ^integer avg-bonus
    ^string info
    ])

  (deftable revenue-table ^{revenue})

  ; ==== bonus table ====
  (defrecord bonus
    [
     ^address token-id
     ^address account
     ^address revenue-id
     ^integer shares
     ^integer amount
     ])

  (deftable bonus-table ^{bonus})


  ;; ==== issue table ====
  (defrecord issues
    [
    ^address token-id
    ^address account
    ^string tx-id
    ^integer issue-shares
    ^integer issue-price 
    ^string info
    ])

  (deftable issue-table ^{issues})

  (defn- new-token
    [token-id token type sendable info]
    (enforce-keyset 'admin-keyset)
    (with-default-read tokens-table token-id 
      {:price -1}
      {price :price}
      (enforce (= price -1) "token exists already")
      (insert tokens-table token-id
              {:token token,
               :type type,
               :info info,
               :sendable sendable,
               :tx-id (tx-hash),
               :price 0,
               :total-shares 0,
               :available-shares 0})
      ))

  (defn new [data]
    (let [meta (+ data {:sendable true,:info ""})
          token-id (get meta :id)
          token (get meta :name)
          type (get meta :type)
          info (get meta :info)
          sendable (if (get meta :sendable) "1" "0")]
      (new-token token-id token type sendable info)
      ))


  (defn- issue-token
    [token-id account issue-shares issue-price info is-revoker]
    (enforce-keyset 'admin-keyset)
    (enforce (< 0 issue-shares) "issue-shares must be a positive integer")
    ; (enforce (<= 0 issue-price) "issue-price must be a positive integer")
    (with-default-read tokens-table token-id
      {:price -1,:total-shares 0,:available-shares 0,:info ""}
      {price :price ,total-shares :total-shares, available-shares :available-shares,old-info :info}
      (enforce (not= -1 price) "token-id not exists")
      (update tokens-table token-id
              {:total-shares (+ total-shares issue-shares),
               :available-shares (+ available-shares issue-shares),
               :price (if (< issue-price 0) price issue-price),
               :info (if (= info "") old-info info)})
      (let [holder-id (hash [token-id account])
            tx-id (tx-hash)
            ]
        (with-default-read stakeholder-table holder-id
          {:shares -1}
          {shares :shares}
          (if (= shares -1)
            (insert stakeholder-table holder-id {:shares issue-shares ,
                                                 :token-id token-id ,
                                                 :holder account, 
                                                 :is-revoker is-revoker,
                                                 :is-owner "1"})
            (update stakeholder-table holder-id {:shares (+ shares issue-shares), 
                                                 :is-revoker is-revoker,
                                                 :is-owner "1"})
            )
          )
        (insert issue-table (hash [token-id tx-id]) {:token-id token-id, 
                                                     :account account, 
                                                     :tx-id tx-id ,
                                                     :issue-shares issue-shares ,
                                                     :issue-price issue-price,
                                                     :info info})
        )
      ))


  (defn issue [data]
    (let [meta (+ data {:info "",:issue-price -1,:is-revoker true})
          token-id (get meta :id)
          account (get meta :account)
          issue-shares (get meta :issue-shares)
          issue-price (get meta :issue-price)
          is-revoker (if (get meta :is-revoker) "1" "0")          
          info (get meta :info)
          ]
      (issue-token token-id account issue-shares issue-price info is-revoker)
      ))


  (defn- dividend-one 
    [account token-id revenue-id avg-bonus holder]
    (let [amount (* avg-bonus (get holder :shares))
          holder-account (get holder :holder)
          bonus-id (hash [holder-account revenue-id])
          ]
      (if (= account holder-account) "" (gd-send account holder-account amount))
      (insert bonus-table bonus-id {:token-id token-id, 
                                    :revenue-id revenue-id,
                                    :account holder-account ,
                                    :shares (get holder :shares) ,
                                    :amount amount})
      amount))



  (defn- dividend-token 
    [token-id avg-bonus account info]
    (enforce-key account)
    (enforce (< 0 avg-bonus) "avg-bonus must be a positive integer")
    (with-read tokens-table token-id
      {available-shares :available-shares, total-shares:total-shares}
      (let [total-bonus (* avg-bonus total-shares)
            tx-id       (tx-hash)
            revenue-id  (hash [token-id tx-id])
            accounts (select stakeholder-table 
                             [:token-id :holder :is-owner :shares] 
                             (and (where :shares (< 0)) (where :token-id (= token-id))))
            total (reduce (+) 0 (map
                                 (dividend-one account token-id revenue-id avg-bonus)
                                 accounts))]
        (enforce (= total total-bonus) "bonus not completely")
        (insert revenue-table revenue-id {:token-id     token-id,
                                          :from         account,
                                          :total-shares total-shares,
                                          :total-bonus  total-bonus,
                                          :avg-bonus    avg-bonus,
                                          :info         info,
                                          :tx-id        tx-id}))
      ))

  (defn dividend [data]
    (let [token-id (get data :id)
          avg-bonus (get data :avg-bonus)
          account (get data :account)
          info (get data :info)
          ]
      (dividend-token token-id avg-bonus account info)
      ))

  (defn- check-sendable 
    [sendable from-is-owner from-is-revoker to-is-owner to-is-revoker]
      (enforce-one "token must sendable or either of 'from' or 'to' must be owner"
                   [(enforce (= sendable "1") "token sendable")
                    (enforce (= from-is-owner "1") "owner can send to anyone")
                    (enforce (and (= from-is-owner "0") (= to-is-revoker "1")) "client only can send to revoker")]
                   ))

  (defn- transfer
    [token-id from to amount sendable]
    (let [from-id (hash [token-id from])
          to-id   (hash  [token-id to])]
      (with-default-read stakeholder-table from-id
        {:shares -1, :is-owner "0",:is-revoker "0"}
        {from-shares :shares, from-is-owner :is-owner,from-is-revoker :is-revoker}
        (with-default-read stakeholder-table to-id
          {:shares -1,:is-owner "0",:is-revoker "0"}
          {to-shares :shares, to-is-owner :is-owner,to-is-revoker :is-revoker}
          (check-sendable sendable from-is-owner from-is-revoker to-is-owner to-is-revoker)
          (enforce (>= from-shares amount) "Insufficient shares for transfer")
          (update stakeholder-table from-id {:shares (- from-shares amount)})
          (if (= to-shares -1)
            (insert stakeholder-table to-id {:shares   amount,
                                             :token-id token-id,
                                             :is-owner "0",
                                             :is-revoker "0",
                                             :holder   to})
            (update stakeholder-table to-id {:shares (+ to-shares amount)}))
          ))
      ))

  (defn- send-token
    [token-id from to amount]
    (enforce-key from)
    (enforce (not= to from) "Can't send to yourself")
    (enforce (< 0 amount) "Amount must be a positive integer")
    (with-default-read tokens-table token-id
      {:price -1,:sendable "0"}
      {price :price,sendable :sendable}
      (enforce (not= -1 price) "token-id not exists")
      (transfer token-id from to amount sendable)
      ))

  (defn send [data]
    (let [token-id (get data :id)
          from (get data :from)
          to (get data :to)
          amount (get data :amount)]
      (send-token token-id from to amount)))

  (defn balance
    [account]
    (enforce-keyset 'admin-keyset)
    (select stakeholder-table [:token-id :shares ]
            (where :holder (= account)))
    )

  (defn set-meta [data]
    (enforce-keyset 'admin-keyset)
    (let [meta (+ data {:name "",:type "",:info "" :sendable ""})
          token-id (get meta :id)
          token-name (get meta :name)
          token-type (get meta :type)
          token-info (get meta :info)
          sendable (get meta :sendable)
          ]
      (with-default-read tokens-table token-id
        {:token "",:type "",:info "" :sendable "" ,:price -1}
        {old-name :token,old-type :type,old-info :info,old-send :sendable ,old-price :price}
        (enforce (not= -1 old-price) "token-id not exists")
        (update tokens-table token-id {:token (if (= token-name "") old-name token-name),
                                       :type (if (= token-type "") old-type token-type),
                                       :info (if (= token-info "") old-info token-info),
                                       :sendable (if (= sendable "") old-send sendable)
                                       }
                )
        )
      ))

  (defn set-holder [data]
    (enforce-keyset 'admin-keyset)
    (let [account (get data :account)
          token-id (get data :id)
          holder-id (hash [token-id account])
          revoker (get data :is-revoker)
          is-revoker (if revoker "1" "0")
          is-owner (if revoker "1" (if (get data :is-owner) "1" "0"))
          ]
      (with-default-read stakeholder-table holder-id
        {:is-owner "0", :is-revoker "0" ,:shares -1}
        {old-owner :is-owner,old-revoker :is-revoker,old-shares :shares}
        (if (= old-shares -1)
          (insert stakeholder-table holder-id {:shares 0,
                                               :token-id token-id,
                                               :is-owner is-owner,
                                               :is-revoker is-revoker,
                                               :holder   account})
          (update stakeholder-table holder-id {:is-owner is-owner,
                                               :is-revoker is-revoker}))
        )
      ))

(create-table tokens-table)
(create-table stakeholder-table)
(create-table revenue-table)
(create-table bonus-table)
(create-table issue-table)
|]


tokenResult :: TF.Result [Exp Parsed]
tokenResult = TF.parseString exprsOnly mempty tokenSrc

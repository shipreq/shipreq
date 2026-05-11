-------------------------------------------------- MODULE encryption --------------------------------------------------

(*
LEGEND: <key>:<encrypted content>
        i.e. dataKey:data means an encrypted blob which when decrypted with dataKey returns data

Client                  Server
  |                        |
  | NewProject             |
  |----------------------> |
  | <----------------------|
  |                 secret |
  |                        |
  | + keyKey               |
  | + dataKey              |
  |                        |
  | dataKey:data           |
  | keyKey:dataKey         |
  | keyKey:secret          |
  |----------------------> |


Client                  Server
  |                        |
  | keyKey                 |
  |                        |
  | ReadProject            |
  |----------------------> |
  | <----------------------|
  |                 secret |
  |                        |
  | keyKey:secret          |
  |----------------------> |
  |                     ok |
  | <----------------------|
  |           dataKey:data |
  |         keyKey:dataKey |
  |                        |
  | dataKey                |
  | data                   |
  |                        |
  |========================|
  |                        |
  | WriteProject           |
  |                        |
  | + data2                |
  |                        |
  | dataKey:data2          |
  |----------------------> |


Client                  Server
  |                        |
  | keyKey                 |
  |                        |
  | ReplaceKey             |
  |----------------------> |
  | <----------------------|
  |                 secret |
  |                        |
  | keyKey:secret          |
  |----------------------> |
  |                     ok |
  | <----------------------|
  |         keyKey:dataKey |
  |                secret2 |
  |                        |
  | dataKey                |
  | + keyKey2              |
  |                        |
  | keyKey:secret          |
  | keyKey2:dataKey        |
  | keyKey2:secret2        |
  |----------------------> |
*)

EXTENDS Sequences, TLC

CONSTANTS Data,
          Keys,
          Secrets,
          Users

VARIABLES serverSeen,   \* Keys & secrets the server has ever had undecrypted access to
          userSeen,     \* Keys & secrets each user has ever had undecrypted access to
          pcReplaceKey,

          \* Server-side state:
          data,         \* User data (project content) decrypted with dataKey
          key,          \* dataKey decrypted with keyKey
          secret,       \* secret (undecrypted)
          keyKeySecret, \* secret decrypted with keyKey
          oldSecrets    \* Previously used secrets (undecrypted)

vars == << serverSeen, userSeen, pcReplaceKey, data, key, secret, keyKeySecret, oldSecrets >>

UsedSecrets        == oldSecrets ++ {secret}
UsersWithoutKeyKey == {u \in Users : key.key \notin userSeen[u]}
UsersWithKeyKey    == {u \in Users : key.key \in userSeen[u]}

-----------------------------------------------------------------------------------------------------------------------

TypeInvariants ==
  & serverSeen   \in SUBSET (Keys ++ Secrets)
  & userSeen     \in [Users -> SUBSET (Keys ++ Secrets ++ [decrypted: Secrets ++ Keys, key: Keys])]
  & pcReplaceKey \in [Users -> [old: Secrets, new: Secrets, active: {TRUE}] ++ [active: {FALSE}]]
  & data         \in [decrypted: {Data},  key: Keys] \* This is a blob which if decrypted with .key, would produce .decrypted
  & key          \in [decrypted: Keys,    key: Keys] \* This is a blob which if decrypted with .key, would produce .decrypted
  & keyKeySecret \in [decrypted: Secrets, key: Keys]
  & secret       \in Secrets
  & oldSecrets   \in SUBSET Secrets

KeyKeyUnlocksDataKey ==
  key.decrypted = data.key

ValueInvariants ==
  & keyKeySecret.key = key.key
  & keyKeySecret.decrypted = secret
  \* debug
  \*/\ PrintT([serverSeen |-> serverSeen, userSeen |-> userSeen, pcReplaceKey |-> pcReplaceKey, data |-> data, key |-> key, secret |-> secret, keyKeySecret |-> keyKeySecret, oldSecrets |-> oldSecrets])

SanityChecksT ==
  & serverSeen \subseteq serverSeen'
  & \A u \in Users : userSeen[u] \subseteq userSeen'[u]

SanityChecks == [][SanityChecksT]_<<vars>>

-----------------------------------------------------------------------------------------------------------------------

UserSees(u, s) == userSeen' = [userSeen EXCEPT ![u] = @ ++ s]
ServerSees(s)  == serverSeen' = serverSeen ++ s

-----------------------------------------------------------------------------------------------------------------------

Init ==
  LET u         == CHOOSE u \in Users : TRUE
      dataKey   == CHOOSE k \in Keys  : TRUE
      keyKey    == CHOOSE k \in Keys  : TRUE
      encData   == [decrypted |-> Data,    key |-> dataKey]
      encKey    == [decrypted |-> dataKey, key |-> keyKey]
      encSecret == [decrypted |-> secret,  key |-> keyKey]
  IN
    & secret       = CHOOSE s \in Secrets : TRUE
    & serverSeen   = {secret}
    & data         = encData
    & key          = encKey
    & keyKeySecret = encSecret
    & userSeen     = [i \in Users |-> IF i = u THEN {dataKey, keyKey, secret, encKey, encSecret} ELSE {}]
    & oldSecrets   = {}
    & pcReplaceKey = [i \in Users |-> [active |-> FALSE]]

ReadProject(u) ==
  & | key.key \in userSeen[u]
     | keyKeySecret \in userSeen[u] & data.key \in userSeen[u]
  & UserSees(u, {secret, keyKeySecret, data.key})
  & UNCHANGED << serverSeen, pcReplaceKey, data, key, secret, keyKeySecret, oldSecrets >>

\* Users can share secrets between themselves offline
\* In terms of keyKeys, that's expected and recommended.
\* In terms of secrets & dataKeys, those are hacking attempts.
UsersShareSecrets ==
  \E u1 \in Users :
  \E s  \in userSeen[u1] :
  \E u2 \in Users :
    & u1 != u2
    & s \notin userSeen[u2]
    & UserSees(u2, {s})
    & UNCHANGED << serverSeen, pcReplaceKey, data, key, secret, keyKeySecret, oldSecrets >>

ReplaceKey1(u) ==
  LET seen      == userSeen[u]
      hasKeyKey == key.key \in seen
      secret2   == CHOOSE s \in Secrets : s \notin UsedSecrets
      seenDK2   == IF hasKeyKey THEN {data.key} ELSE {}
  IN
    & | hasKeyKey
       | keyKeySecret \in seen & data.key \in seen
    & UserSees(u, {secret2} ++ seenDK2)
    & ServerSees({secret2})
    & pcReplaceKey' = [pcReplaceKey EXCEPT ![u] = [old |-> secret, new |-> secret2, active |-> TRUE]]
    & UNCHANGED << data, key, secret, keyKeySecret, oldSecrets >>

ReplaceKey2(u) ==
  LET s == pcReplaceKey[u] IN
  & s.active
  & \E keyKey1 \in Keys \intersect userSeen[u] :
     \E keyKey2 \in Keys :
     \E dataKey2 \in Keys : \* Either user decrypts actual dataKey using keyKey (expected), or attacker uses bullshit
       & LET UserKeyKeySecret == [decrypted |-> s.old, key |-> keyKey1]
          IN
            & keyKeySecret = UserKeyKeySecret \* server-side assertion
            & UserSees(u, {keyKey2, dataKey2, s.new})
            & secret'       = s.new
            & keyKeySecret' = [decrypted |-> s.new  ,  key |-> keyKey2]
            & key'          = [decrypted |-> dataKey2, key |-> keyKey2]
            & oldSecrets'   = oldSecrets ++ {secret}
            & pcReplaceKey' = [pcReplaceKey EXCEPT ![u] = [active |-> FALSE]]
            & UNCHANGED << serverSeen, data >>
(*
  TODO Server needs
    - to verify that keyKey2:dataKey decrypts to dataKey
    - knowing
      - keyKey:dataKey
      - keyKey2:dataKey
    - without knowing
      - dataKey
      - keyKey
      - keyKey2

  TODO Server needs
    - knowing
      - K1:D1
      - K2:D2
    - without knowing
      - D1
      - D2
      - K1
      - K2
    - to test whether D1 = D2

Say Alice has two encryption keys (K1, K2), and two data (D1, D2).
Now say Bob has encrypted copies of Alice's data (D1/K1, D2/K2) and isn't allowed to know any of {K1, K2, D1, D2}.
How can Bob and Alice interact such that Bob can test whether D1 = D2?
Or put another way, how can Alice prove to Bob that D1 is/isn't D2.

*)


Next ==
  | UsersShareSecrets
  | \E u \in Users :
    | ReadProject(u)
    | ReplaceKey1(u)
    | ReplaceKey2(u)

-----------------------------------------------------------------------------------------------------------------------

SafeFromServer ==
  LET CanDecryptData == data.key \in serverSeen
      CanDecryptKey  == key.key \in serverSeen
  IN ~(CanDecryptData | CanDecryptKey)

SafeFromUsersWithoutKeyKey ==
  & \A u \in UsersWithoutKeyKey :
    & ~ENABLED(ReadProject(u))

OpenToUsersWithKeyKey ==
  & \A u \in UsersWithKeyKey :
    & ENABLED(ReadProject(u))

-----------------------------------------------------------------------------------------------------------------------

Spec == Init & [][Next]_<<vars>>

========================================================================================================================

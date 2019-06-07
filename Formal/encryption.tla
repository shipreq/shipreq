-------------------------------------------------- MODULE encryption --------------------------------------------------

(* PROTOCOL
   - If users send their key to the server to assert it's valid, they'll violate SafeFromServer
   - Maybe server sends [encrypted: 'secret123', key: K] and if the user sends back 'secret123' they we know they have a valid key
     'secret123' would be initially encrypted by client in NewProject and send as plain text to server
     'secret123' would be to be tracked in a Seen variable to ensure that they can't use it later to avoid their key becoming invalid
     If every key change requires a new secret then I think we're good - would have to retain all old secrets and ensure no reuse on key change

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
  |          keyKey:secret |
  |                        |
  | secret                 |
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
  | + data'                |
  |                        |
  | dataKey:data'          |
  |----------------------> |


Client                  Server
  |                        |
  | keyKey                 |
  |                        |
  | ReplaceKey             |
  |----------------------> |
  | <----------------------|
  |          keyKey:secret |
  |                        |
  | secret                 |
  |----------------------> |
  |                     ok |
  | <----------------------|
  |         keyKey:dataKey |
  |                secret' |
  |                        |
  | dataKey                |
  | + keyKey'              |
  |                        |
  | keyKey':dataKey        |
  | keyKey':secret'        |
  |----------------------> |
*)

EXTENDS Sequences, TLC

CONSTANTS Data,
          Keys,
          Secrets,
          Users
          
VARIABLES serverSeen,  \* Keys & secrets the server has ever had unencrypted access to
          userSeen,    \* Keys & secrets each user has ever had unencrypted access to

          \* Server-side state:
          data,        \* User data (project content) encrypted with dataKey
          key,         \* dataKey encrypted with keyKey
          secret,      \* secret (unencrypted)
          secretE,     \* secret encrypted with keyKey
          oldSecrets   \* Previously used secrets (unencrypted)

vars == << serverSeen, userSeen, data, key, secret, secretE, oldSecrets >>

UsersWithoutKeyKey == {u \in Users : key.key \notin userSeen[u]}
UsersWithKeyKey    == {u \in Users : key.key \in userSeen[u]}

-----------------------------------------------------------------------------------------------------------------------

TypeInvariants ==
  /\ serverSeen \in SUBSET (Keys \union Secrets)
  /\ userSeen   \in [Users -> SUBSET (Keys \union Secrets)]
  /\ data       \in [encrypted: {Data},  key: Keys] \* This is a blob which if decrypted with .key, would produce .encrypted
  /\ key        \in [encrypted: Keys,    key: Keys] \* This is a blob which if decrypted with .key, would produce .encrypted
  /\ secretE    \in [encrypted: Secrets, key: Keys]
  /\ secret     \in Secrets
  /\ oldSecrets \in SUBSET Secrets

ValueInvariants ==
  /\ key.encrypted = data.key   \* keyKey unlocks dataKey
  /\ secretE.key = key.key      \* secretE encrypted by keyKey
  /\ secretE.encrypted = secret \* secret & secretE are the same secret
  \* debug
  /\ PrintT([serverSeen |-> serverSeen, userSeen |-> userSeen, data |-> data, key |-> key, secret |-> secret, secretE |-> secretE, oldSecrets |-> oldSecrets])

SanityChecksT ==
  /\ serverSeen \subseteq serverSeen'
  /\ \A u \in Users : userSeen[u] \subseteq userSeen'[u]

SanityChecks == [][SanityChecksT]_<<vars>>

-----------------------------------------------------------------------------------------------------------------------

UserSees(u, s) == userSeen' = [userSeen EXCEPT ![u] = @ \union s]
ServerSees(s)  == serverSeen' = serverSeen \union s

-----------------------------------------------------------------------------------------------------------------------

Init ==
  LET u       == CHOOSE u \in Users : TRUE
      dataKey == CHOOSE k \in Keys  : TRUE
      keyKey  == CHOOSE k \in Keys  : k /= dataKey
  IN
    /\ secret     = CHOOSE s \in Secrets : TRUE
    /\ serverSeen = {secret}
    /\ data       = [encrypted |-> Data,    key |-> dataKey]
    /\ key        = [encrypted |-> dataKey, key |-> keyKey]
    /\ secretE    = [encrypted |-> secret,  key |-> keyKey]
    /\ userSeen   = [i \in Users |-> IF i = u THEN {dataKey, keyKey, secret} ELSE {}]
    /\ oldSecrets = {}

ReadProjectU(u) ==
  /\ \/ key.key \in userSeen[u]
     \/ secret \in userSeen[u] /\ data.key \in userSeen[u]
  /\ UserSees(u, {data.key, secret})
  /\ UNCHANGED << serverSeen, data, key, secret, secretE, oldSecrets >>

ReadProject ==
  \E u \in Users : ReadProjectU(u)

\* Users can share secrets between themselves offline
\* In terms of keyKeys, that's expected and recommended.
\* In terms of secrets & dataKeys, those are hacking attempts.
UsersShareSecrets ==
  \E u1 \in Users :
  \E s  \in userSeen[u1] :
  \E u2 \in Users :
    /\ u1 /= u2
    /\ s \notin userSeen[u2]
    /\ UserSees(u2, {s})
    /\ UNCHANGED << serverSeen, data, key, secret, secretE, oldSecrets >>

Next ==
  \/ ReadProject
  \/ UsersShareSecrets
  
-----------------------------------------------------------------------------------------------------------------------

SafeFromServer ==
  LET CanDecryptData == data.key \in serverSeen
      CanDecryptKey  == key.key \in serverSeen
  IN ~(CanDecryptData \/ CanDecryptKey)

SafeFromUsersWithoutKeyKey ==
  /\ \A u \in UsersWithoutKeyKey :
    /\ ~ENABLED(ReadProjectU(u))

OpenToUsersWithKeyKey ==
  /\ \A u \in UsersWithKeyKey :
    /\ ENABLED(ReadProjectU(u))

\* The only TLA+ operator that can produce a non-symmetric expression when applied to a symmetric expression is CHOOSE
\* MCSymmetry == nope

Spec == Init /\ [][Next]_<<vars>>

========================================================================================================================

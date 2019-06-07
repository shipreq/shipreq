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
          
VARIABLES serverSeen,    \* Keys & secrets the server has ever had unencrypted access to
          userSeen,      \* Keys & secrets each user has ever had unencrypted access to
          dbData,        \* User data (project content) encrypted with dataKey
          dbKey,         \* dataKey encrypted with keyKey
          dbSecret,      \* secret (unencrypted)
          dbSecretE,     \* secret encrypted with keyKey
          dbOldSecrets   \* Previously used secrets (unencrypted)

vars == << serverSeen, userSeen, dbData, dbKey, dbSecret, dbSecretE, dbOldSecrets >>

UsersWithoutKeyKey == {u \in Users : dbKey.key \notin userSeen[u]}

UsersWithKeyKey == {u \in Users : dbKey.key \in userSeen[u]}

IsEncryptedBy(e, key) == e.key = key

-----------------------------------------------------------------------------------------------------------------------

TypeInvariants ==
  /\ serverSeen   \in SUBSET (Keys \union Secrets)
  /\ userSeen     \in [Users -> SUBSET (Keys \union Secrets)]
  /\ dbData       \in [encrypted: {Data},  key: Keys] \* This is a blob which if decrypted with .key, would produce .encrypted
  /\ dbKey        \in [encrypted: Keys,    key: Keys] \* This is a blob which if decrypted with .key, would produce .encrypted
  /\ dbSecretE    \in [encrypted: Secrets, key: Keys]
  /\ dbSecret     \in Secrets
  /\ dbOldSecrets \in SUBSET Secrets

ValueInvariants ==
  /\ dbData.key = dbKey.encrypted   \* keyKey unlocks dataKey
  /\ dbKey.key = dbSecretE.key      \* dataKey & secretE encrypted by same key
  /\ dbSecretE.encrypted = dbSecret \* dbSecret & dbSecretE are the same secret
  /\ PrintT([serverSeen |-> serverSeen, userSeen |-> userSeen, dbData |-> dbData, dbKey |-> dbKey, dbSecret |-> dbSecret, dbSecretE |-> dbSecretE, dbOldSecrets |-> dbOldSecrets]) \* debug

SanityChecksT ==
  /\ serverSeen \subseteq serverSeen'
  /\ \A u \in Users : userSeen[u] \subseteq userSeen'[u]

SanityChecks == [][SanityChecksT]_<<vars>>
  
SafeFromServer ==
  LET CanDecryptData == dbData.key \in serverSeen
      CanDecryptKey  == dbKey.key \in serverSeen
  IN ~(CanDecryptData \/ CanDecryptKey)

SafeFromUsersWithoutKeyKey ==
  /\ \A u \in UsersWithoutKeyKey :
    /\ TRUE \* TODO: Depends on the protocol.

OpenToUsersWithKeyKey ==
  /\ \A u \in UsersWithKeyKey :
    /\ TRUE \* TODO: Depends on the protocol.

-----------------------------------------------------------------------------------------------------------------------

UserSees(u, s) == userSeen' = [userSeen EXCEPT ![u] = @ \union s]
ServerSees(s)  == serverSeen' = serverSeen \union s

-----------------------------------------------------------------------------------------------------------------------

Init ==
  LET u       == CHOOSE u \in Users : TRUE
      secret  == CHOOSE s \in Secrets : TRUE
      dataKey == CHOOSE k \in Keys : TRUE
      keyKey  == CHOOSE k \in Keys : k /= dataKey
  IN
    /\ dbSecret     = secret
    /\ serverSeen   = {secret}
    /\ dbData       = [encrypted |-> Data,    key |-> dataKey]
    /\ dbKey        = [encrypted |-> dataKey, key |-> keyKey]
    /\ dbSecretE    = [encrypted |-> secret,  key |-> keyKey]
    /\ userSeen     = [i \in Users |-> IF i = u THEN {dataKey, keyKey, secret} ELSE {}]
    /\ dbOldSecrets = {}

Next ==
  \/ FALSE
  
-----------------------------------------------------------------------------------------------------------------------

\* The only TLA+ operator that can produce a non-symmetric expression when applied to a symmetric expression is CHOOSE
\* MCSymmetry == Permutations(Keys) \union Permutations(Users)

Spec == Init /\ [][Next]_<<vars>>

========================================================================================================================

-------------------------------------------------- MODULE encryption --------------------------------------------------

EXTENDS Sequences, TLC

CONSTANTS None,
          Keys,
          Data,
          Users
          
VARIABLES serverSeen,
          userSeen,
          dbData,
          dbKey

vars == << serverSeen, userSeen, dbData, dbKey >>

UsersWithoutKeyKey ==
  IF dbData = None
  THEN Users
  ELSE {u \in Users : dbKey.key \notin userSeen[u]}

UsersWithKeyKey ==
  IF dbData = None
  THEN {}
  ELSE {u \in Users : dbKey.key \in userSeen[u]}

-----------------------------------------------------------------------------------------------------------------------

TypeInvariants ==
  /\ serverSeen \in SUBSET Keys
  /\ userSeen   \in [Users -> SUBSET Keys]
  /\ dbData     \in [encrypted: {Data}, key: Keys] \union {None} \* This is a blob which if decrypted with .key, would produce .encrypted
  /\ dbKey      \in [encrypted: Keys,   key: Keys] \union {None} \* This is a blob which if decrypted with .key, would produce .encrypted

ValueInvariants ==
  /\ (dbKey /= None) => (dbKey.encrypted = dbData.key)
  /\ PrintT([serverSeen |-> serverSeen, userSeen |-> userSeen, dbData |-> dbData, dbKey |-> dbKey])

SanityChecksF ==
  /\ serverSeen \subseteq serverSeen'
  /\ \A u \in Users : userSeen[u] \subseteq userSeen'[u]

SanityChecks == [][SanityChecksF]_<<vars>>
  
SafeFromServer ==
  LET CanDecryptData == dbData.key \in serverSeen
      CanDecryptKey  == dbKey.key \in serverSeen
      Safe           == ~(CanDecryptData \/ CanDecryptKey)
  IN (dbData /= None) => Safe  

SafeFromUsersWithoutKeyKey ==
  /\ dbData /= None
  /\ \A u \in UsersWithoutKeyKey :
    /\ TRUE \* TODO: Depends on the protocol.

OpenToUsersWithKeyKey ==
  /\ \A u \in UsersWithKeyKey :
    /\ TRUE \* TODO: Depends on the protocol.

(* PROTOCOL
   - If users send their key to the server to assert it's valid, they'll violate SafeFromServer
   - Maybe server sends [encrypted: 'secret123', key: K] and if the user sends back 'secret123' they we know they have a valid key
     'secret123' would be initially encrypted by client in NewProject and send as plain text to server
     'secret123' would be to be tracked in a Seen variable to ensure that they can't use it later to avoid their key becoming invalid
     If every key change requires a new secret then I think we're good - would have to retain all old secrets and ensure no reuse on key change 
*)

-----------------------------------------------------------------------------------------------------------------------

UserSees(u, newKeys) ==
  userSeen' = [userSeen EXCEPT ![u] = @ \union newKeys]

-----------------------------------------------------------------------------------------------------------------------

Init ==
  /\ serverSeen = {}
  /\ userSeen   = [u \in Users |-> {}]
  /\ dbData     = None
  /\ dbKey      = None

NewProject ==
  /\ dbData = None
  /\ \E u \in Users:
    LET dataKey == CHOOSE k \in Keys : TRUE
        keyKey  == CHOOSE k \in Keys : k /= dataKey
    IN
      /\ dbData' = [encrypted |-> Data,    key |-> dataKey]
      /\ dbKey'  = [encrypted |-> dataKey, key |-> keyKey]
      /\ UserSees(u, {dataKey, keyKey})
      /\ UNCHANGED << serverSeen >>

\*ReadKey(k) ==
\*  /\ k \in user
\*  /\ key.key = k
\*  /\ user' = user \union {key.value}
\*  

Next ==
  \/ NewProject
  
-----------------------------------------------------------------------------------------------------------------------

\* The only TLA+ operator that can produce a non-symmetric expression when applied to a symmetric expression is CHOOSE
\* MCSymmetry == Permutations(Keys) \union Permutations(Users)

Spec == Init /\ [][Next]_<<vars>>

========================================================================================================================
\*  /\ data' = [value |-> Data, key |-> Key1]

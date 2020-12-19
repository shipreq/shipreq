---------------------------------------------------- MODULE Network ----------------------------------------------------

EXTENDS Naturals, Sequences, Util

VARIABLE queue

Invariants(Msg) ==
  queue \in Seq(Msg)

Init ==
  queue = <<>>

------------------------------------------------------------------------------------------------------------------------

IsEmpty ==
  Len(queue) = 0

\* Communication channels between a source and target are not commutative; we can rely on the order not changing.
LOCAL IsNextPerChannel(i, sameChannel(_, _)) ==
  LET n      == queue[i]
      isNext == \A j \in 1..(i-1) : ~sameChannel(n, queue[j])
  IN i = 1 | (i != 0 & isNext)

LOCAL SameFromAndTo(n, m) ==
  (n.from = m.from) & (n.to = m.to)

\* Input:
\*   pred        :: Msg        => Boolean -- whether or not a Msg is one you want to find
\*   sameChannel :: (Msg, Msg) => Boolean -- whether two messages are on the same channel (i.e. same to/from)
\* Output:
\*   Set[Index]
NextMsgsPerChannelWhere(pred(_), sameChannel(_, _)) ==
  { i \in DOMAIN queue : pred(queue[i]) & IsNextPerChannel(i, sameChannel) }

\* Convenience method for NextMsgsPerChannelWhere where
\*   pred        = _.type = type
\*   sameChannel = (n,m) => (n.from = m.from) && (n.to = m.to)
NextMsgsByType(type) ==
  NextMsgsPerChannelWhere(
    LAMBDA m: m.type = type,
    SameFromAndTo)

\* Convenience method for NextMsgsPerChannelWhere where
\*   pred        = _ => true
\*   sameChannel = (n,m) => (n.from = m.from) && (n.to = m.to)
NextMsgs ==
  { i \in DOMAIN queue : IsNextPerChannel(i, SameFromAndTo) }

NothingInFlightFrom(from) ==
  ~SeqExists(queue, LAMBDA m: m.from = from)

NothingInFlightFromTo(from, to) ==
  ~SeqExists(queue, LAMBDA m: m.from = from & m.to = to)

ModRecv       (q, i)       == RemoveAt(q, i)
ModSend       (q, msg)     == Append(q, msg)
ModSendSet    (q, msgs)    == SetFold(msgs, q, Append)
ModRecvSend   (q, i, msg)  == ModSend(ModRecv(q, i), msg)
ModRecvSendSeq(q, i, msgs) == ModRecv(q, i) \o msgs
ModRecvSendSet(q, i, msgs) == ModSendSet(ModRecv(q, i), msgs)

Recv       (i)       == queue' = ModRecv       (queue, i)
Send       (msg)     == queue' = ModSend       (queue, msg)
SendSet    (msgs)    == queue' = ModSendSet    (queue, msgs)
RecvSend   (i, msg)  == queue' = ModRecvSend   (queue, i, msg)
RecvSendSeq(i, msgs) == queue' = ModRecvSendSeq(queue, i, msgs)
RecvSendSet(i, msgs) == queue' = ModRecvSendSet(queue, i, msgs)

========================================================================================================================

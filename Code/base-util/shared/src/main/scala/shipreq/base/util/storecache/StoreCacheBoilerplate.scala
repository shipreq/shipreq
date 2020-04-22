package shipreq.base.util.storecache

private[storecache] final class StoreCache2[In, SA,A, SB,B, Z](
    val s1: StoreCache1[In, SA, A],
    val s2: StoreCache1[In, SB, B],
    mapOut: (A,B) => Z) extends StoreCache[In, Z] {

  type Self[II, ZZ] = StoreCache2[II, SA,A, SB,B, ZZ]

  override lazy val value: Z =
    mapOut(s1.value, s2.value)

  override def contramap[X](ff: X => In): Self[X, Z] =
    new StoreCache2(s1.contramap(ff), s2.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new StoreCache2(s1, s2, (a,b) => ff(mapOut(a,b)))
}

private[storecache] final class StoreCache3[In, SA,A, SB,B, SC,C, Z](
    val s1: StoreCache1[In, SA, A],
    val s2: StoreCache1[In, SB, B],
    val s3: StoreCache1[In, SC, C],
    mapOut: (A,B,C) => Z) extends StoreCache[In, Z] {

  type Self[II, ZZ] = StoreCache3[II, SA,A, SB,B, SC,C, ZZ]

  override lazy val value: Z =
    mapOut(s1.value, s2.value, s3.value)

  override def contramap[X](ff: X => In): Self[X, Z] =
    new StoreCache3(s1.contramap(ff), s2.contramap(ff), s3.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new StoreCache3(s1, s2, s3, (a,b,c) => ff(mapOut(a,b,c)))
}

private[storecache] final class StoreCache4[In, SA,A, SB,B, SC,C, SD,D, Z](
    val s1: StoreCache1[In, SA, A],
    val s2: StoreCache1[In, SB, B],
    val s3: StoreCache1[In, SC, C],
    val s4: StoreCache1[In, SD, D],
    mapOut: (A,B,C,D) => Z) extends StoreCache[In, Z] {

  type Self[II, ZZ] = StoreCache4[II, SA,A, SB,B, SC,C, SD,D, ZZ]

  override lazy val value: Z =
    mapOut(s1.value, s2.value, s3.value, s4.value)

  override def contramap[X](ff: X => In): Self[X, Z] =
    new StoreCache4(s1.contramap(ff), s2.contramap(ff), s3.contramap(ff), s4.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new StoreCache4(s1, s2, s3, s4, (a,b,c,d) => ff(mapOut(a,b,c,d)))
}

private[storecache] final class StoreCache5[In, SA,A, SB,B, SC,C, SD,D, SE,E, Z](
    val s1: StoreCache1[In, SA, A],
    val s2: StoreCache1[In, SB, B],
    val s3: StoreCache1[In, SC, C],
    val s4: StoreCache1[In, SD, D],
    val s5: StoreCache1[In, SE, E],
    mapOut: (A,B,C,D,E) => Z) extends StoreCache[In, Z] {

  type Self[II, ZZ] = StoreCache5[II, SA,A, SB,B, SC,C, SD,D, SE,E, ZZ]

  override lazy val value: Z =
    mapOut(s1.value, s2.value, s3.value, s4.value, s5.value)

  override def contramap[X](ff: X => In): Self[X, Z] =
    new StoreCache5(s1.contramap(ff), s2.contramap(ff), s3.contramap(ff), s4.contramap(ff), s5.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new StoreCache5(s1, s2, s3, s4, s5, (a,b,c,d,e) => ff(mapOut(a,b,c,d,e)))
}

private[storecache] final class StoreCache6[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, Z](
    val s1: StoreCache1[In, SA, A],
    val s2: StoreCache1[In, SB, B],
    val s3: StoreCache1[In, SC, C],
    val s4: StoreCache1[In, SD, D],
    val s5: StoreCache1[In, SE, E],
    val s6: StoreCache1[In, SF, F],
    mapOut: (A,B,C,D,E,F) => Z) extends StoreCache[In, Z] {

  type Self[II, ZZ] = StoreCache6[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, ZZ]

  override lazy val value: Z =
    mapOut(s1.value, s2.value, s3.value, s4.value, s5.value, s6.value)

  override def contramap[X](ff: X => In): Self[X, Z] =
    new StoreCache6(s1.contramap(ff), s2.contramap(ff), s3.contramap(ff), s4.contramap(ff), s5.contramap(ff), s6.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new StoreCache6(s1, s2, s3, s4, s5, s6, (a,b,c,d,e,f) => ff(mapOut(a,b,c,d,e,f)))
}

private[storecache] final class StoreCache7[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, Z](
    val s1: StoreCache1[In, SA, A],
    val s2: StoreCache1[In, SB, B],
    val s3: StoreCache1[In, SC, C],
    val s4: StoreCache1[In, SD, D],
    val s5: StoreCache1[In, SE, E],
    val s6: StoreCache1[In, SF, F],
    val s7: StoreCache1[In, SG, G],
    mapOut: (A,B,C,D,E,F,G) => Z) extends StoreCache[In, Z] {

  type Self[II, ZZ] = StoreCache7[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, ZZ]

  override lazy val value: Z =
    mapOut(s1.value, s2.value, s3.value, s4.value, s5.value, s6.value, s7.value)

  override def contramap[X](ff: X => In): Self[X, Z] =
    new StoreCache7(s1.contramap(ff), s2.contramap(ff), s3.contramap(ff), s4.contramap(ff), s5.contramap(ff), s6.contramap(ff), s7.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new StoreCache7(s1, s2, s3, s4, s5, s6, s7, (a,b,c,d,e,f,g) => ff(mapOut(a,b,c,d,e,f,g)))
}

private[storecache] final class StoreCache8[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, Z](
    val s1: StoreCache1[In, SA, A],
    val s2: StoreCache1[In, SB, B],
    val s3: StoreCache1[In, SC, C],
    val s4: StoreCache1[In, SD, D],
    val s5: StoreCache1[In, SE, E],
    val s6: StoreCache1[In, SF, F],
    val s7: StoreCache1[In, SG, G],
    val s8: StoreCache1[In, SH, H],
    mapOut: (A,B,C,D,E,F,G,H) => Z) extends StoreCache[In, Z] {

  type Self[II, ZZ] = StoreCache8[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, ZZ]

  override lazy val value: Z =
    mapOut(s1.value, s2.value, s3.value, s4.value, s5.value, s6.value, s7.value, s8.value)

  override def contramap[X](ff: X => In): Self[X, Z] =
    new StoreCache8(s1.contramap(ff), s2.contramap(ff), s3.contramap(ff), s4.contramap(ff), s5.contramap(ff), s6.contramap(ff), s7.contramap(ff), s8.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new StoreCache8(s1, s2, s3, s4, s5, s6, s7, s8, (a,b,c,d,e,f,g,h) => ff(mapOut(a,b,c,d,e,f,g,h)))
}

private[storecache] final class StoreCache9[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, Z](
    val s1: StoreCache1[In, SA, A],
    val s2: StoreCache1[In, SB, B],
    val s3: StoreCache1[In, SC, C],
    val s4: StoreCache1[In, SD, D],
    val s5: StoreCache1[In, SE, E],
    val s6: StoreCache1[In, SF, F],
    val s7: StoreCache1[In, SG, G],
    val s8: StoreCache1[In, SH, H],
    val s9: StoreCache1[In, SI, I],
    mapOut: (A,B,C,D,E,F,G,H,I) => Z) extends StoreCache[In, Z] {

  type Self[II, ZZ] = StoreCache9[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, ZZ]

  override lazy val value: Z =
    mapOut(s1.value, s2.value, s3.value, s4.value, s5.value, s6.value, s7.value, s8.value, s9.value)

  override def contramap[X](ff: X => In): Self[X, Z] =
    new StoreCache9(s1.contramap(ff), s2.contramap(ff), s3.contramap(ff), s4.contramap(ff), s5.contramap(ff), s6.contramap(ff), s7.contramap(ff), s8.contramap(ff), s9.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new StoreCache9(s1, s2, s3, s4, s5, s6, s7, s8, s9, (a,b,c,d,e,f,g,h,i) => ff(mapOut(a,b,c,d,e,f,g,h,i)))
}

private[storecache] final class StoreCache10[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, Z](
    val s1: StoreCache1[In, SA, A],
    val s2: StoreCache1[In, SB, B],
    val s3: StoreCache1[In, SC, C],
    val s4: StoreCache1[In, SD, D],
    val s5: StoreCache1[In, SE, E],
    val s6: StoreCache1[In, SF, F],
    val s7: StoreCache1[In, SG, G],
    val s8: StoreCache1[In, SH, H],
    val s9: StoreCache1[In, SI, I],
    val s10: StoreCache1[In, SJ, J],
    mapOut: (A,B,C,D,E,F,G,H,I,J) => Z) extends StoreCache[In, Z] {

  type Self[II, ZZ] = StoreCache10[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, ZZ]

  override lazy val value: Z =
    mapOut(s1.value, s2.value, s3.value, s4.value, s5.value, s6.value, s7.value, s8.value, s9.value, s10.value)

  override def contramap[X](ff: X => In): Self[X, Z] =
    new StoreCache10(s1.contramap(ff), s2.contramap(ff), s3.contramap(ff), s4.contramap(ff), s5.contramap(ff), s6.contramap(ff), s7.contramap(ff), s8.contramap(ff), s9.contramap(ff), s10.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new StoreCache10(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, (a,b,c,d,e,f,g,h,i,j) => ff(mapOut(a,b,c,d,e,f,g,h,i,j)))
}

private[storecache] final class StoreCache11[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, Z](
    val s1: StoreCache1[In, SA, A],
    val s2: StoreCache1[In, SB, B],
    val s3: StoreCache1[In, SC, C],
    val s4: StoreCache1[In, SD, D],
    val s5: StoreCache1[In, SE, E],
    val s6: StoreCache1[In, SF, F],
    val s7: StoreCache1[In, SG, G],
    val s8: StoreCache1[In, SH, H],
    val s9: StoreCache1[In, SI, I],
    val s10: StoreCache1[In, SJ, J],
    val s11: StoreCache1[In, SK, K],
    mapOut: (A,B,C,D,E,F,G,H,I,J,K) => Z) extends StoreCache[In, Z] {

  type Self[II, ZZ] = StoreCache11[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, ZZ]

  override lazy val value: Z =
    mapOut(s1.value, s2.value, s3.value, s4.value, s5.value, s6.value, s7.value, s8.value, s9.value, s10.value, s11.value)

  override def contramap[X](ff: X => In): Self[X, Z] =
    new StoreCache11(s1.contramap(ff), s2.contramap(ff), s3.contramap(ff), s4.contramap(ff), s5.contramap(ff), s6.contramap(ff), s7.contramap(ff), s8.contramap(ff), s9.contramap(ff), s10.contramap(ff), s11.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new StoreCache11(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, (a,b,c,d,e,f,g,h,i,j,k) => ff(mapOut(a,b,c,d,e,f,g,h,i,j,k)))
}

private[storecache] final class StoreCache12[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, Z](
    val s1: StoreCache1[In, SA, A],
    val s2: StoreCache1[In, SB, B],
    val s3: StoreCache1[In, SC, C],
    val s4: StoreCache1[In, SD, D],
    val s5: StoreCache1[In, SE, E],
    val s6: StoreCache1[In, SF, F],
    val s7: StoreCache1[In, SG, G],
    val s8: StoreCache1[In, SH, H],
    val s9: StoreCache1[In, SI, I],
    val s10: StoreCache1[In, SJ, J],
    val s11: StoreCache1[In, SK, K],
    val s12: StoreCache1[In, SL, L],
    mapOut: (A,B,C,D,E,F,G,H,I,J,K,L) => Z) extends StoreCache[In, Z] {

  type Self[II, ZZ] = StoreCache12[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, ZZ]

  override lazy val value: Z =
    mapOut(s1.value, s2.value, s3.value, s4.value, s5.value, s6.value, s7.value, s8.value, s9.value, s10.value, s11.value, s12.value)

  override def contramap[X](ff: X => In): Self[X, Z] =
    new StoreCache12(s1.contramap(ff), s2.contramap(ff), s3.contramap(ff), s4.contramap(ff), s5.contramap(ff), s6.contramap(ff), s7.contramap(ff), s8.contramap(ff), s9.contramap(ff), s10.contramap(ff), s11.contramap(ff), s12.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new StoreCache12(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, (a,b,c,d,e,f,g,h,i,j,k,l) => ff(mapOut(a,b,c,d,e,f,g,h,i,j,k,l)))
}

private[storecache] final class StoreCache13[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, Z](
    val s1: StoreCache1[In, SA, A],
    val s2: StoreCache1[In, SB, B],
    val s3: StoreCache1[In, SC, C],
    val s4: StoreCache1[In, SD, D],
    val s5: StoreCache1[In, SE, E],
    val s6: StoreCache1[In, SF, F],
    val s7: StoreCache1[In, SG, G],
    val s8: StoreCache1[In, SH, H],
    val s9: StoreCache1[In, SI, I],
    val s10: StoreCache1[In, SJ, J],
    val s11: StoreCache1[In, SK, K],
    val s12: StoreCache1[In, SL, L],
    val s13: StoreCache1[In, SM, M],
    mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M) => Z) extends StoreCache[In, Z] {

  type Self[II, ZZ] = StoreCache13[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, ZZ]

  override lazy val value: Z =
    mapOut(s1.value, s2.value, s3.value, s4.value, s5.value, s6.value, s7.value, s8.value, s9.value, s10.value, s11.value, s12.value, s13.value)

  override def contramap[X](ff: X => In): Self[X, Z] =
    new StoreCache13(s1.contramap(ff), s2.contramap(ff), s3.contramap(ff), s4.contramap(ff), s5.contramap(ff), s6.contramap(ff), s7.contramap(ff), s8.contramap(ff), s9.contramap(ff), s10.contramap(ff), s11.contramap(ff), s12.contramap(ff), s13.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new StoreCache13(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, (a,b,c,d,e,f,g,h,i,j,k,l,m) => ff(mapOut(a,b,c,d,e,f,g,h,i,j,k,l,m)))
}

private[storecache] final class StoreCache14[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, Z](
    val s1: StoreCache1[In, SA, A],
    val s2: StoreCache1[In, SB, B],
    val s3: StoreCache1[In, SC, C],
    val s4: StoreCache1[In, SD, D],
    val s5: StoreCache1[In, SE, E],
    val s6: StoreCache1[In, SF, F],
    val s7: StoreCache1[In, SG, G],
    val s8: StoreCache1[In, SH, H],
    val s9: StoreCache1[In, SI, I],
    val s10: StoreCache1[In, SJ, J],
    val s11: StoreCache1[In, SK, K],
    val s12: StoreCache1[In, SL, L],
    val s13: StoreCache1[In, SM, M],
    val s14: StoreCache1[In, SN, N],
    mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N) => Z) extends StoreCache[In, Z] {

  type Self[II, ZZ] = StoreCache14[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, ZZ]

  override lazy val value: Z =
    mapOut(s1.value, s2.value, s3.value, s4.value, s5.value, s6.value, s7.value, s8.value, s9.value, s10.value, s11.value, s12.value, s13.value, s14.value)

  override def contramap[X](ff: X => In): Self[X, Z] =
    new StoreCache14(s1.contramap(ff), s2.contramap(ff), s3.contramap(ff), s4.contramap(ff), s5.contramap(ff), s6.contramap(ff), s7.contramap(ff), s8.contramap(ff), s9.contramap(ff), s10.contramap(ff), s11.contramap(ff), s12.contramap(ff), s13.contramap(ff), s14.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new StoreCache14(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, (a,b,c,d,e,f,g,h,i,j,k,l,m,n) => ff(mapOut(a,b,c,d,e,f,g,h,i,j,k,l,m,n)))
}

private[storecache] final class StoreCache15[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, Z](
    val s1: StoreCache1[In, SA, A],
    val s2: StoreCache1[In, SB, B],
    val s3: StoreCache1[In, SC, C],
    val s4: StoreCache1[In, SD, D],
    val s5: StoreCache1[In, SE, E],
    val s6: StoreCache1[In, SF, F],
    val s7: StoreCache1[In, SG, G],
    val s8: StoreCache1[In, SH, H],
    val s9: StoreCache1[In, SI, I],
    val s10: StoreCache1[In, SJ, J],
    val s11: StoreCache1[In, SK, K],
    val s12: StoreCache1[In, SL, L],
    val s13: StoreCache1[In, SM, M],
    val s14: StoreCache1[In, SN, N],
    val s15: StoreCache1[In, SO, O],
    mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) => Z) extends StoreCache[In, Z] {

  type Self[II, ZZ] = StoreCache15[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, ZZ]

  override lazy val value: Z =
    mapOut(s1.value, s2.value, s3.value, s4.value, s5.value, s6.value, s7.value, s8.value, s9.value, s10.value, s11.value, s12.value, s13.value, s14.value, s15.value)

  override def contramap[X](ff: X => In): Self[X, Z] =
    new StoreCache15(s1.contramap(ff), s2.contramap(ff), s3.contramap(ff), s4.contramap(ff), s5.contramap(ff), s6.contramap(ff), s7.contramap(ff), s8.contramap(ff), s9.contramap(ff), s10.contramap(ff), s11.contramap(ff), s12.contramap(ff), s13.contramap(ff), s14.contramap(ff), s15.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new StoreCache15(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) => ff(mapOut(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)))
}

private[storecache] final class StoreCache16[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, Z](
    val s1: StoreCache1[In, SA, A],
    val s2: StoreCache1[In, SB, B],
    val s3: StoreCache1[In, SC, C],
    val s4: StoreCache1[In, SD, D],
    val s5: StoreCache1[In, SE, E],
    val s6: StoreCache1[In, SF, F],
    val s7: StoreCache1[In, SG, G],
    val s8: StoreCache1[In, SH, H],
    val s9: StoreCache1[In, SI, I],
    val s10: StoreCache1[In, SJ, J],
    val s11: StoreCache1[In, SK, K],
    val s12: StoreCache1[In, SL, L],
    val s13: StoreCache1[In, SM, M],
    val s14: StoreCache1[In, SN, N],
    val s15: StoreCache1[In, SO, O],
    val s16: StoreCache1[In, SP, P],
    mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) => Z) extends StoreCache[In, Z] {

  type Self[II, ZZ] = StoreCache16[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, ZZ]

  override lazy val value: Z =
    mapOut(s1.value, s2.value, s3.value, s4.value, s5.value, s6.value, s7.value, s8.value, s9.value, s10.value, s11.value, s12.value, s13.value, s14.value, s15.value, s16.value)

  override def contramap[X](ff: X => In): Self[X, Z] =
    new StoreCache16(s1.contramap(ff), s2.contramap(ff), s3.contramap(ff), s4.contramap(ff), s5.contramap(ff), s6.contramap(ff), s7.contramap(ff), s8.contramap(ff), s9.contramap(ff), s10.contramap(ff), s11.contramap(ff), s12.contramap(ff), s13.contramap(ff), s14.contramap(ff), s15.contramap(ff), s16.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new StoreCache16(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) => ff(mapOut(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)))
}

private[storecache] final class StoreCache17[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, Z](
    val s1: StoreCache1[In, SA, A],
    val s2: StoreCache1[In, SB, B],
    val s3: StoreCache1[In, SC, C],
    val s4: StoreCache1[In, SD, D],
    val s5: StoreCache1[In, SE, E],
    val s6: StoreCache1[In, SF, F],
    val s7: StoreCache1[In, SG, G],
    val s8: StoreCache1[In, SH, H],
    val s9: StoreCache1[In, SI, I],
    val s10: StoreCache1[In, SJ, J],
    val s11: StoreCache1[In, SK, K],
    val s12: StoreCache1[In, SL, L],
    val s13: StoreCache1[In, SM, M],
    val s14: StoreCache1[In, SN, N],
    val s15: StoreCache1[In, SO, O],
    val s16: StoreCache1[In, SP, P],
    val s17: StoreCache1[In, SQ, Q],
    mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) => Z) extends StoreCache[In, Z] {

  type Self[II, ZZ] = StoreCache17[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, ZZ]

  override lazy val value: Z =
    mapOut(s1.value, s2.value, s3.value, s4.value, s5.value, s6.value, s7.value, s8.value, s9.value, s10.value, s11.value, s12.value, s13.value, s14.value, s15.value, s16.value, s17.value)

  override def contramap[X](ff: X => In): Self[X, Z] =
    new StoreCache17(s1.contramap(ff), s2.contramap(ff), s3.contramap(ff), s4.contramap(ff), s5.contramap(ff), s6.contramap(ff), s7.contramap(ff), s8.contramap(ff), s9.contramap(ff), s10.contramap(ff), s11.contramap(ff), s12.contramap(ff), s13.contramap(ff), s14.contramap(ff), s15.contramap(ff), s16.contramap(ff), s17.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new StoreCache17(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) => ff(mapOut(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)))
}

private[storecache] final class StoreCache18[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, Z](
    val s1: StoreCache1[In, SA, A],
    val s2: StoreCache1[In, SB, B],
    val s3: StoreCache1[In, SC, C],
    val s4: StoreCache1[In, SD, D],
    val s5: StoreCache1[In, SE, E],
    val s6: StoreCache1[In, SF, F],
    val s7: StoreCache1[In, SG, G],
    val s8: StoreCache1[In, SH, H],
    val s9: StoreCache1[In, SI, I],
    val s10: StoreCache1[In, SJ, J],
    val s11: StoreCache1[In, SK, K],
    val s12: StoreCache1[In, SL, L],
    val s13: StoreCache1[In, SM, M],
    val s14: StoreCache1[In, SN, N],
    val s15: StoreCache1[In, SO, O],
    val s16: StoreCache1[In, SP, P],
    val s17: StoreCache1[In, SQ, Q],
    val s18: StoreCache1[In, SR, R],
    mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) => Z) extends StoreCache[In, Z] {

  type Self[II, ZZ] = StoreCache18[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, ZZ]

  override lazy val value: Z =
    mapOut(s1.value, s2.value, s3.value, s4.value, s5.value, s6.value, s7.value, s8.value, s9.value, s10.value, s11.value, s12.value, s13.value, s14.value, s15.value, s16.value, s17.value, s18.value)

  override def contramap[X](ff: X => In): Self[X, Z] =
    new StoreCache18(s1.contramap(ff), s2.contramap(ff), s3.contramap(ff), s4.contramap(ff), s5.contramap(ff), s6.contramap(ff), s7.contramap(ff), s8.contramap(ff), s9.contramap(ff), s10.contramap(ff), s11.contramap(ff), s12.contramap(ff), s13.contramap(ff), s14.contramap(ff), s15.contramap(ff), s16.contramap(ff), s17.contramap(ff), s18.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new StoreCache18(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) => ff(mapOut(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)))
}

private[storecache] final class StoreCache19[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, Z](
    val s1: StoreCache1[In, SA, A],
    val s2: StoreCache1[In, SB, B],
    val s3: StoreCache1[In, SC, C],
    val s4: StoreCache1[In, SD, D],
    val s5: StoreCache1[In, SE, E],
    val s6: StoreCache1[In, SF, F],
    val s7: StoreCache1[In, SG, G],
    val s8: StoreCache1[In, SH, H],
    val s9: StoreCache1[In, SI, I],
    val s10: StoreCache1[In, SJ, J],
    val s11: StoreCache1[In, SK, K],
    val s12: StoreCache1[In, SL, L],
    val s13: StoreCache1[In, SM, M],
    val s14: StoreCache1[In, SN, N],
    val s15: StoreCache1[In, SO, O],
    val s16: StoreCache1[In, SP, P],
    val s17: StoreCache1[In, SQ, Q],
    val s18: StoreCache1[In, SR, R],
    val s19: StoreCache1[In, SS, S],
    mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) => Z) extends StoreCache[In, Z] {

  type Self[II, ZZ] = StoreCache19[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, ZZ]

  override lazy val value: Z =
    mapOut(s1.value, s2.value, s3.value, s4.value, s5.value, s6.value, s7.value, s8.value, s9.value, s10.value, s11.value, s12.value, s13.value, s14.value, s15.value, s16.value, s17.value, s18.value, s19.value)

  override def contramap[X](ff: X => In): Self[X, Z] =
    new StoreCache19(s1.contramap(ff), s2.contramap(ff), s3.contramap(ff), s4.contramap(ff), s5.contramap(ff), s6.contramap(ff), s7.contramap(ff), s8.contramap(ff), s9.contramap(ff), s10.contramap(ff), s11.contramap(ff), s12.contramap(ff), s13.contramap(ff), s14.contramap(ff), s15.contramap(ff), s16.contramap(ff), s17.contramap(ff), s18.contramap(ff), s19.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new StoreCache19(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) => ff(mapOut(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)))
}

private[storecache] final class StoreCache20[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, ST,T, Z](
    val s1: StoreCache1[In, SA, A],
    val s2: StoreCache1[In, SB, B],
    val s3: StoreCache1[In, SC, C],
    val s4: StoreCache1[In, SD, D],
    val s5: StoreCache1[In, SE, E],
    val s6: StoreCache1[In, SF, F],
    val s7: StoreCache1[In, SG, G],
    val s8: StoreCache1[In, SH, H],
    val s9: StoreCache1[In, SI, I],
    val s10: StoreCache1[In, SJ, J],
    val s11: StoreCache1[In, SK, K],
    val s12: StoreCache1[In, SL, L],
    val s13: StoreCache1[In, SM, M],
    val s14: StoreCache1[In, SN, N],
    val s15: StoreCache1[In, SO, O],
    val s16: StoreCache1[In, SP, P],
    val s17: StoreCache1[In, SQ, Q],
    val s18: StoreCache1[In, SR, R],
    val s19: StoreCache1[In, SS, S],
    val s20: StoreCache1[In, ST, T],
    mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) => Z) extends StoreCache[In, Z] {

  type Self[II, ZZ] = StoreCache20[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, ST,T, ZZ]

  override lazy val value: Z =
    mapOut(s1.value, s2.value, s3.value, s4.value, s5.value, s6.value, s7.value, s8.value, s9.value, s10.value, s11.value, s12.value, s13.value, s14.value, s15.value, s16.value, s17.value, s18.value, s19.value, s20.value)

  override def contramap[X](ff: X => In): Self[X, Z] =
    new StoreCache20(s1.contramap(ff), s2.contramap(ff), s3.contramap(ff), s4.contramap(ff), s5.contramap(ff), s6.contramap(ff), s7.contramap(ff), s8.contramap(ff), s9.contramap(ff), s10.contramap(ff), s11.contramap(ff), s12.contramap(ff), s13.contramap(ff), s14.contramap(ff), s15.contramap(ff), s16.contramap(ff), s17.contramap(ff), s18.contramap(ff), s19.contramap(ff), s20.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new StoreCache20(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) => ff(mapOut(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)))
}

private[storecache] final class StoreCache21[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, ST,T, SU,U, Z](
    val s1: StoreCache1[In, SA, A],
    val s2: StoreCache1[In, SB, B],
    val s3: StoreCache1[In, SC, C],
    val s4: StoreCache1[In, SD, D],
    val s5: StoreCache1[In, SE, E],
    val s6: StoreCache1[In, SF, F],
    val s7: StoreCache1[In, SG, G],
    val s8: StoreCache1[In, SH, H],
    val s9: StoreCache1[In, SI, I],
    val s10: StoreCache1[In, SJ, J],
    val s11: StoreCache1[In, SK, K],
    val s12: StoreCache1[In, SL, L],
    val s13: StoreCache1[In, SM, M],
    val s14: StoreCache1[In, SN, N],
    val s15: StoreCache1[In, SO, O],
    val s16: StoreCache1[In, SP, P],
    val s17: StoreCache1[In, SQ, Q],
    val s18: StoreCache1[In, SR, R],
    val s19: StoreCache1[In, SS, S],
    val s20: StoreCache1[In, ST, T],
    val s21: StoreCache1[In, SU, U],
    mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) => Z) extends StoreCache[In, Z] {

  type Self[II, ZZ] = StoreCache21[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, ST,T, SU,U, ZZ]

  override lazy val value: Z =
    mapOut(s1.value, s2.value, s3.value, s4.value, s5.value, s6.value, s7.value, s8.value, s9.value, s10.value, s11.value, s12.value, s13.value, s14.value, s15.value, s16.value, s17.value, s18.value, s19.value, s20.value, s21.value)

  override def contramap[X](ff: X => In): Self[X, Z] =
    new StoreCache21(s1.contramap(ff), s2.contramap(ff), s3.contramap(ff), s4.contramap(ff), s5.contramap(ff), s6.contramap(ff), s7.contramap(ff), s8.contramap(ff), s9.contramap(ff), s10.contramap(ff), s11.contramap(ff), s12.contramap(ff), s13.contramap(ff), s14.contramap(ff), s15.contramap(ff), s16.contramap(ff), s17.contramap(ff), s18.contramap(ff), s19.contramap(ff), s20.contramap(ff), s21.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new StoreCache21(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) => ff(mapOut(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)))
}

private[storecache] final class StoreCache22[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, ST,T, SU,U, SV,V, Z](
    val s1: StoreCache1[In, SA, A],
    val s2: StoreCache1[In, SB, B],
    val s3: StoreCache1[In, SC, C],
    val s4: StoreCache1[In, SD, D],
    val s5: StoreCache1[In, SE, E],
    val s6: StoreCache1[In, SF, F],
    val s7: StoreCache1[In, SG, G],
    val s8: StoreCache1[In, SH, H],
    val s9: StoreCache1[In, SI, I],
    val s10: StoreCache1[In, SJ, J],
    val s11: StoreCache1[In, SK, K],
    val s12: StoreCache1[In, SL, L],
    val s13: StoreCache1[In, SM, M],
    val s14: StoreCache1[In, SN, N],
    val s15: StoreCache1[In, SO, O],
    val s16: StoreCache1[In, SP, P],
    val s17: StoreCache1[In, SQ, Q],
    val s18: StoreCache1[In, SR, R],
    val s19: StoreCache1[In, SS, S],
    val s20: StoreCache1[In, ST, T],
    val s21: StoreCache1[In, SU, U],
    val s22: StoreCache1[In, SV, V],
    mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) => Z) extends StoreCache[In, Z] {

  type Self[II, ZZ] = StoreCache22[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, ST,T, SU,U, SV,V, ZZ]

  override lazy val value: Z =
    mapOut(s1.value, s2.value, s3.value, s4.value, s5.value, s6.value, s7.value, s8.value, s9.value, s10.value, s11.value, s12.value, s13.value, s14.value, s15.value, s16.value, s17.value, s18.value, s19.value, s20.value, s21.value, s22.value)

  override def contramap[X](ff: X => In): Self[X, Z] =
    new StoreCache22(s1.contramap(ff), s2.contramap(ff), s3.contramap(ff), s4.contramap(ff), s5.contramap(ff), s6.contramap(ff), s7.contramap(ff), s8.contramap(ff), s9.contramap(ff), s10.contramap(ff), s11.contramap(ff), s12.contramap(ff), s13.contramap(ff), s14.contramap(ff), s15.contramap(ff), s16.contramap(ff), s17.contramap(ff), s18.contramap(ff), s19.contramap(ff), s20.contramap(ff), s21.contramap(ff), s22.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new StoreCache22(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) => ff(mapOut(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v)))
}

abstract class StoreCacheBoilerplate private[storecache]() {

  final def apply2[In, SA,A, SB,B, Z](
      s1: StoreCache1[In, SA, A],
      s2: StoreCache1[In, SB, B])(
      mapOut: (A,B) => Z): StoreCache[In, Z] =
    new StoreCache2(s1, s2, mapOut)

  final def apply3[In, SA,A, SB,B, SC,C, Z](
      s1: StoreCache1[In, SA, A],
      s2: StoreCache1[In, SB, B],
      s3: StoreCache1[In, SC, C])(
      mapOut: (A,B,C) => Z): StoreCache[In, Z] =
    new StoreCache3(s1, s2, s3, mapOut)

  final def apply4[In, SA,A, SB,B, SC,C, SD,D, Z](
      s1: StoreCache1[In, SA, A],
      s2: StoreCache1[In, SB, B],
      s3: StoreCache1[In, SC, C],
      s4: StoreCache1[In, SD, D])(
      mapOut: (A,B,C,D) => Z): StoreCache[In, Z] =
    new StoreCache4(s1, s2, s3, s4, mapOut)

  final def apply5[In, SA,A, SB,B, SC,C, SD,D, SE,E, Z](
      s1: StoreCache1[In, SA, A],
      s2: StoreCache1[In, SB, B],
      s3: StoreCache1[In, SC, C],
      s4: StoreCache1[In, SD, D],
      s5: StoreCache1[In, SE, E])(
      mapOut: (A,B,C,D,E) => Z): StoreCache[In, Z] =
    new StoreCache5(s1, s2, s3, s4, s5, mapOut)

  final def apply6[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, Z](
      s1: StoreCache1[In, SA, A],
      s2: StoreCache1[In, SB, B],
      s3: StoreCache1[In, SC, C],
      s4: StoreCache1[In, SD, D],
      s5: StoreCache1[In, SE, E],
      s6: StoreCache1[In, SF, F])(
      mapOut: (A,B,C,D,E,F) => Z): StoreCache[In, Z] =
    new StoreCache6(s1, s2, s3, s4, s5, s6, mapOut)

  final def apply7[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, Z](
      s1: StoreCache1[In, SA, A],
      s2: StoreCache1[In, SB, B],
      s3: StoreCache1[In, SC, C],
      s4: StoreCache1[In, SD, D],
      s5: StoreCache1[In, SE, E],
      s6: StoreCache1[In, SF, F],
      s7: StoreCache1[In, SG, G])(
      mapOut: (A,B,C,D,E,F,G) => Z): StoreCache[In, Z] =
    new StoreCache7(s1, s2, s3, s4, s5, s6, s7, mapOut)

  final def apply8[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, Z](
      s1: StoreCache1[In, SA, A],
      s2: StoreCache1[In, SB, B],
      s3: StoreCache1[In, SC, C],
      s4: StoreCache1[In, SD, D],
      s5: StoreCache1[In, SE, E],
      s6: StoreCache1[In, SF, F],
      s7: StoreCache1[In, SG, G],
      s8: StoreCache1[In, SH, H])(
      mapOut: (A,B,C,D,E,F,G,H) => Z): StoreCache[In, Z] =
    new StoreCache8(s1, s2, s3, s4, s5, s6, s7, s8, mapOut)

  final def apply9[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, Z](
      s1: StoreCache1[In, SA, A],
      s2: StoreCache1[In, SB, B],
      s3: StoreCache1[In, SC, C],
      s4: StoreCache1[In, SD, D],
      s5: StoreCache1[In, SE, E],
      s6: StoreCache1[In, SF, F],
      s7: StoreCache1[In, SG, G],
      s8: StoreCache1[In, SH, H],
      s9: StoreCache1[In, SI, I])(
      mapOut: (A,B,C,D,E,F,G,H,I) => Z): StoreCache[In, Z] =
    new StoreCache9(s1, s2, s3, s4, s5, s6, s7, s8, s9, mapOut)

  final def apply10[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, Z](
      s1: StoreCache1[In, SA, A],
      s2: StoreCache1[In, SB, B],
      s3: StoreCache1[In, SC, C],
      s4: StoreCache1[In, SD, D],
      s5: StoreCache1[In, SE, E],
      s6: StoreCache1[In, SF, F],
      s7: StoreCache1[In, SG, G],
      s8: StoreCache1[In, SH, H],
      s9: StoreCache1[In, SI, I],
      s10: StoreCache1[In, SJ, J])(
      mapOut: (A,B,C,D,E,F,G,H,I,J) => Z): StoreCache[In, Z] =
    new StoreCache10(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, mapOut)

  final def apply11[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, Z](
      s1: StoreCache1[In, SA, A],
      s2: StoreCache1[In, SB, B],
      s3: StoreCache1[In, SC, C],
      s4: StoreCache1[In, SD, D],
      s5: StoreCache1[In, SE, E],
      s6: StoreCache1[In, SF, F],
      s7: StoreCache1[In, SG, G],
      s8: StoreCache1[In, SH, H],
      s9: StoreCache1[In, SI, I],
      s10: StoreCache1[In, SJ, J],
      s11: StoreCache1[In, SK, K])(
      mapOut: (A,B,C,D,E,F,G,H,I,J,K) => Z): StoreCache[In, Z] =
    new StoreCache11(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, mapOut)

  final def apply12[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, Z](
      s1: StoreCache1[In, SA, A],
      s2: StoreCache1[In, SB, B],
      s3: StoreCache1[In, SC, C],
      s4: StoreCache1[In, SD, D],
      s5: StoreCache1[In, SE, E],
      s6: StoreCache1[In, SF, F],
      s7: StoreCache1[In, SG, G],
      s8: StoreCache1[In, SH, H],
      s9: StoreCache1[In, SI, I],
      s10: StoreCache1[In, SJ, J],
      s11: StoreCache1[In, SK, K],
      s12: StoreCache1[In, SL, L])(
      mapOut: (A,B,C,D,E,F,G,H,I,J,K,L) => Z): StoreCache[In, Z] =
    new StoreCache12(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, mapOut)

  final def apply13[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, Z](
      s1: StoreCache1[In, SA, A],
      s2: StoreCache1[In, SB, B],
      s3: StoreCache1[In, SC, C],
      s4: StoreCache1[In, SD, D],
      s5: StoreCache1[In, SE, E],
      s6: StoreCache1[In, SF, F],
      s7: StoreCache1[In, SG, G],
      s8: StoreCache1[In, SH, H],
      s9: StoreCache1[In, SI, I],
      s10: StoreCache1[In, SJ, J],
      s11: StoreCache1[In, SK, K],
      s12: StoreCache1[In, SL, L],
      s13: StoreCache1[In, SM, M])(
      mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M) => Z): StoreCache[In, Z] =
    new StoreCache13(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, mapOut)

  final def apply14[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, Z](
      s1: StoreCache1[In, SA, A],
      s2: StoreCache1[In, SB, B],
      s3: StoreCache1[In, SC, C],
      s4: StoreCache1[In, SD, D],
      s5: StoreCache1[In, SE, E],
      s6: StoreCache1[In, SF, F],
      s7: StoreCache1[In, SG, G],
      s8: StoreCache1[In, SH, H],
      s9: StoreCache1[In, SI, I],
      s10: StoreCache1[In, SJ, J],
      s11: StoreCache1[In, SK, K],
      s12: StoreCache1[In, SL, L],
      s13: StoreCache1[In, SM, M],
      s14: StoreCache1[In, SN, N])(
      mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N) => Z): StoreCache[In, Z] =
    new StoreCache14(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, mapOut)

  final def apply15[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, Z](
      s1: StoreCache1[In, SA, A],
      s2: StoreCache1[In, SB, B],
      s3: StoreCache1[In, SC, C],
      s4: StoreCache1[In, SD, D],
      s5: StoreCache1[In, SE, E],
      s6: StoreCache1[In, SF, F],
      s7: StoreCache1[In, SG, G],
      s8: StoreCache1[In, SH, H],
      s9: StoreCache1[In, SI, I],
      s10: StoreCache1[In, SJ, J],
      s11: StoreCache1[In, SK, K],
      s12: StoreCache1[In, SL, L],
      s13: StoreCache1[In, SM, M],
      s14: StoreCache1[In, SN, N],
      s15: StoreCache1[In, SO, O])(
      mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) => Z): StoreCache[In, Z] =
    new StoreCache15(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, mapOut)

  final def apply16[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, Z](
      s1: StoreCache1[In, SA, A],
      s2: StoreCache1[In, SB, B],
      s3: StoreCache1[In, SC, C],
      s4: StoreCache1[In, SD, D],
      s5: StoreCache1[In, SE, E],
      s6: StoreCache1[In, SF, F],
      s7: StoreCache1[In, SG, G],
      s8: StoreCache1[In, SH, H],
      s9: StoreCache1[In, SI, I],
      s10: StoreCache1[In, SJ, J],
      s11: StoreCache1[In, SK, K],
      s12: StoreCache1[In, SL, L],
      s13: StoreCache1[In, SM, M],
      s14: StoreCache1[In, SN, N],
      s15: StoreCache1[In, SO, O],
      s16: StoreCache1[In, SP, P])(
      mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) => Z): StoreCache[In, Z] =
    new StoreCache16(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, mapOut)

  final def apply17[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, Z](
      s1: StoreCache1[In, SA, A],
      s2: StoreCache1[In, SB, B],
      s3: StoreCache1[In, SC, C],
      s4: StoreCache1[In, SD, D],
      s5: StoreCache1[In, SE, E],
      s6: StoreCache1[In, SF, F],
      s7: StoreCache1[In, SG, G],
      s8: StoreCache1[In, SH, H],
      s9: StoreCache1[In, SI, I],
      s10: StoreCache1[In, SJ, J],
      s11: StoreCache1[In, SK, K],
      s12: StoreCache1[In, SL, L],
      s13: StoreCache1[In, SM, M],
      s14: StoreCache1[In, SN, N],
      s15: StoreCache1[In, SO, O],
      s16: StoreCache1[In, SP, P],
      s17: StoreCache1[In, SQ, Q])(
      mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) => Z): StoreCache[In, Z] =
    new StoreCache17(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, mapOut)

  final def apply18[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, Z](
      s1: StoreCache1[In, SA, A],
      s2: StoreCache1[In, SB, B],
      s3: StoreCache1[In, SC, C],
      s4: StoreCache1[In, SD, D],
      s5: StoreCache1[In, SE, E],
      s6: StoreCache1[In, SF, F],
      s7: StoreCache1[In, SG, G],
      s8: StoreCache1[In, SH, H],
      s9: StoreCache1[In, SI, I],
      s10: StoreCache1[In, SJ, J],
      s11: StoreCache1[In, SK, K],
      s12: StoreCache1[In, SL, L],
      s13: StoreCache1[In, SM, M],
      s14: StoreCache1[In, SN, N],
      s15: StoreCache1[In, SO, O],
      s16: StoreCache1[In, SP, P],
      s17: StoreCache1[In, SQ, Q],
      s18: StoreCache1[In, SR, R])(
      mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) => Z): StoreCache[In, Z] =
    new StoreCache18(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, mapOut)

  final def apply19[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, Z](
      s1: StoreCache1[In, SA, A],
      s2: StoreCache1[In, SB, B],
      s3: StoreCache1[In, SC, C],
      s4: StoreCache1[In, SD, D],
      s5: StoreCache1[In, SE, E],
      s6: StoreCache1[In, SF, F],
      s7: StoreCache1[In, SG, G],
      s8: StoreCache1[In, SH, H],
      s9: StoreCache1[In, SI, I],
      s10: StoreCache1[In, SJ, J],
      s11: StoreCache1[In, SK, K],
      s12: StoreCache1[In, SL, L],
      s13: StoreCache1[In, SM, M],
      s14: StoreCache1[In, SN, N],
      s15: StoreCache1[In, SO, O],
      s16: StoreCache1[In, SP, P],
      s17: StoreCache1[In, SQ, Q],
      s18: StoreCache1[In, SR, R],
      s19: StoreCache1[In, SS, S])(
      mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) => Z): StoreCache[In, Z] =
    new StoreCache19(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, mapOut)

  final def apply20[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, ST,T, Z](
      s1: StoreCache1[In, SA, A],
      s2: StoreCache1[In, SB, B],
      s3: StoreCache1[In, SC, C],
      s4: StoreCache1[In, SD, D],
      s5: StoreCache1[In, SE, E],
      s6: StoreCache1[In, SF, F],
      s7: StoreCache1[In, SG, G],
      s8: StoreCache1[In, SH, H],
      s9: StoreCache1[In, SI, I],
      s10: StoreCache1[In, SJ, J],
      s11: StoreCache1[In, SK, K],
      s12: StoreCache1[In, SL, L],
      s13: StoreCache1[In, SM, M],
      s14: StoreCache1[In, SN, N],
      s15: StoreCache1[In, SO, O],
      s16: StoreCache1[In, SP, P],
      s17: StoreCache1[In, SQ, Q],
      s18: StoreCache1[In, SR, R],
      s19: StoreCache1[In, SS, S],
      s20: StoreCache1[In, ST, T])(
      mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) => Z): StoreCache[In, Z] =
    new StoreCache20(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, mapOut)

  final def apply21[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, ST,T, SU,U, Z](
      s1: StoreCache1[In, SA, A],
      s2: StoreCache1[In, SB, B],
      s3: StoreCache1[In, SC, C],
      s4: StoreCache1[In, SD, D],
      s5: StoreCache1[In, SE, E],
      s6: StoreCache1[In, SF, F],
      s7: StoreCache1[In, SG, G],
      s8: StoreCache1[In, SH, H],
      s9: StoreCache1[In, SI, I],
      s10: StoreCache1[In, SJ, J],
      s11: StoreCache1[In, SK, K],
      s12: StoreCache1[In, SL, L],
      s13: StoreCache1[In, SM, M],
      s14: StoreCache1[In, SN, N],
      s15: StoreCache1[In, SO, O],
      s16: StoreCache1[In, SP, P],
      s17: StoreCache1[In, SQ, Q],
      s18: StoreCache1[In, SR, R],
      s19: StoreCache1[In, SS, S],
      s20: StoreCache1[In, ST, T],
      s21: StoreCache1[In, SU, U])(
      mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) => Z): StoreCache[In, Z] =
    new StoreCache21(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, mapOut)

  final def apply22[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, ST,T, SU,U, SV,V, Z](
      s1: StoreCache1[In, SA, A],
      s2: StoreCache1[In, SB, B],
      s3: StoreCache1[In, SC, C],
      s4: StoreCache1[In, SD, D],
      s5: StoreCache1[In, SE, E],
      s6: StoreCache1[In, SF, F],
      s7: StoreCache1[In, SG, G],
      s8: StoreCache1[In, SH, H],
      s9: StoreCache1[In, SI, I],
      s10: StoreCache1[In, SJ, J],
      s11: StoreCache1[In, SK, K],
      s12: StoreCache1[In, SL, L],
      s13: StoreCache1[In, SM, M],
      s14: StoreCache1[In, SN, N],
      s15: StoreCache1[In, SO, O],
      s16: StoreCache1[In, SP, P],
      s17: StoreCache1[In, SQ, Q],
      s18: StoreCache1[In, SR, R],
      s19: StoreCache1[In, SS, S],
      s20: StoreCache1[In, ST, T],
      s21: StoreCache1[In, SU, U],
      s22: StoreCache1[In, SV, V])(
      mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) => Z): StoreCache[In, Z] =
    new StoreCache22(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, mapOut)
}

private[storecache] final class Logic2[In, SA,A, SB,B, Z](
    l1: Logic1[In, SA, A],
    l2: Logic1[In, SB, B],
    mapOut: (A,B) => Z) extends StoreCache.Logic[In, Z] {

  type Self[II, ZZ] = Logic2[II, SA,A, SB,B, ZZ]

  override type Cache = StoreCache2[In, SA,A, SB,B, Z]

  override def contramap[X](ff: X => In): Self[X, Z] =
    new Logic2(l1.contramap(ff), l2.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new Logic2(l1, l2, (a,b) => ff(mapOut(a,b)))

  override def init(i: In): Cache = {
    val s1 = l1.init(i)
    val s2 = l2.init(i)
    new StoreCache2(s1, s2, mapOut)
  }

  override def next(prev: Cache, i: In): Cache = {
    val x1 = l1.next(prev.s1, i)
    val x2 = l2.next(prev.s2, i)
    if ((x1 eq prev.s1) && (x2 eq prev.s2))
      prev
    else
      new StoreCache2(x1, x2, mapOut)
  }
}

private[storecache] final class Logic3[In, SA,A, SB,B, SC,C, Z](
    l1: Logic1[In, SA, A],
    l2: Logic1[In, SB, B],
    l3: Logic1[In, SC, C],
    mapOut: (A,B,C) => Z) extends StoreCache.Logic[In, Z] {

  type Self[II, ZZ] = Logic3[II, SA,A, SB,B, SC,C, ZZ]

  override type Cache = StoreCache3[In, SA,A, SB,B, SC,C, Z]

  override def contramap[X](ff: X => In): Self[X, Z] =
    new Logic3(l1.contramap(ff), l2.contramap(ff), l3.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new Logic3(l1, l2, l3, (a,b,c) => ff(mapOut(a,b,c)))

  override def init(i: In): Cache = {
    val s1 = l1.init(i)
    val s2 = l2.init(i)
    val s3 = l3.init(i)
    new StoreCache3(s1, s2, s3, mapOut)
  }

  override def next(prev: Cache, i: In): Cache = {
    val x1 = l1.next(prev.s1, i)
    val x2 = l2.next(prev.s2, i)
    val x3 = l3.next(prev.s3, i)
    if ((x1 eq prev.s1) && (x2 eq prev.s2) && (x3 eq prev.s3))
      prev
    else
      new StoreCache3(x1, x2, x3, mapOut)
  }
}

private[storecache] final class Logic4[In, SA,A, SB,B, SC,C, SD,D, Z](
    l1: Logic1[In, SA, A],
    l2: Logic1[In, SB, B],
    l3: Logic1[In, SC, C],
    l4: Logic1[In, SD, D],
    mapOut: (A,B,C,D) => Z) extends StoreCache.Logic[In, Z] {

  type Self[II, ZZ] = Logic4[II, SA,A, SB,B, SC,C, SD,D, ZZ]

  override type Cache = StoreCache4[In, SA,A, SB,B, SC,C, SD,D, Z]

  override def contramap[X](ff: X => In): Self[X, Z] =
    new Logic4(l1.contramap(ff), l2.contramap(ff), l3.contramap(ff), l4.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new Logic4(l1, l2, l3, l4, (a,b,c,d) => ff(mapOut(a,b,c,d)))

  override def init(i: In): Cache = {
    val s1 = l1.init(i)
    val s2 = l2.init(i)
    val s3 = l3.init(i)
    val s4 = l4.init(i)
    new StoreCache4(s1, s2, s3, s4, mapOut)
  }

  override def next(prev: Cache, i: In): Cache = {
    val x1 = l1.next(prev.s1, i)
    val x2 = l2.next(prev.s2, i)
    val x3 = l3.next(prev.s3, i)
    val x4 = l4.next(prev.s4, i)
    if ((x1 eq prev.s1) && (x2 eq prev.s2) && (x3 eq prev.s3) && (x4 eq prev.s4))
      prev
    else
      new StoreCache4(x1, x2, x3, x4, mapOut)
  }
}

private[storecache] final class Logic5[In, SA,A, SB,B, SC,C, SD,D, SE,E, Z](
    l1: Logic1[In, SA, A],
    l2: Logic1[In, SB, B],
    l3: Logic1[In, SC, C],
    l4: Logic1[In, SD, D],
    l5: Logic1[In, SE, E],
    mapOut: (A,B,C,D,E) => Z) extends StoreCache.Logic[In, Z] {

  type Self[II, ZZ] = Logic5[II, SA,A, SB,B, SC,C, SD,D, SE,E, ZZ]

  override type Cache = StoreCache5[In, SA,A, SB,B, SC,C, SD,D, SE,E, Z]

  override def contramap[X](ff: X => In): Self[X, Z] =
    new Logic5(l1.contramap(ff), l2.contramap(ff), l3.contramap(ff), l4.contramap(ff), l5.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new Logic5(l1, l2, l3, l4, l5, (a,b,c,d,e) => ff(mapOut(a,b,c,d,e)))

  override def init(i: In): Cache = {
    val s1 = l1.init(i)
    val s2 = l2.init(i)
    val s3 = l3.init(i)
    val s4 = l4.init(i)
    val s5 = l5.init(i)
    new StoreCache5(s1, s2, s3, s4, s5, mapOut)
  }

  override def next(prev: Cache, i: In): Cache = {
    val x1 = l1.next(prev.s1, i)
    val x2 = l2.next(prev.s2, i)
    val x3 = l3.next(prev.s3, i)
    val x4 = l4.next(prev.s4, i)
    val x5 = l5.next(prev.s5, i)
    if ((x1 eq prev.s1) && (x2 eq prev.s2) && (x3 eq prev.s3) && (x4 eq prev.s4) && (x5 eq prev.s5))
      prev
    else
      new StoreCache5(x1, x2, x3, x4, x5, mapOut)
  }
}

private[storecache] final class Logic6[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, Z](
    l1: Logic1[In, SA, A],
    l2: Logic1[In, SB, B],
    l3: Logic1[In, SC, C],
    l4: Logic1[In, SD, D],
    l5: Logic1[In, SE, E],
    l6: Logic1[In, SF, F],
    mapOut: (A,B,C,D,E,F) => Z) extends StoreCache.Logic[In, Z] {

  type Self[II, ZZ] = Logic6[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, ZZ]

  override type Cache = StoreCache6[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, Z]

  override def contramap[X](ff: X => In): Self[X, Z] =
    new Logic6(l1.contramap(ff), l2.contramap(ff), l3.contramap(ff), l4.contramap(ff), l5.contramap(ff), l6.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new Logic6(l1, l2, l3, l4, l5, l6, (a,b,c,d,e,f) => ff(mapOut(a,b,c,d,e,f)))

  override def init(i: In): Cache = {
    val s1 = l1.init(i)
    val s2 = l2.init(i)
    val s3 = l3.init(i)
    val s4 = l4.init(i)
    val s5 = l5.init(i)
    val s6 = l6.init(i)
    new StoreCache6(s1, s2, s3, s4, s5, s6, mapOut)
  }

  override def next(prev: Cache, i: In): Cache = {
    val x1 = l1.next(prev.s1, i)
    val x2 = l2.next(prev.s2, i)
    val x3 = l3.next(prev.s3, i)
    val x4 = l4.next(prev.s4, i)
    val x5 = l5.next(prev.s5, i)
    val x6 = l6.next(prev.s6, i)
    if ((x1 eq prev.s1) && (x2 eq prev.s2) && (x3 eq prev.s3) && (x4 eq prev.s4) && (x5 eq prev.s5) && (x6 eq prev.s6))
      prev
    else
      new StoreCache6(x1, x2, x3, x4, x5, x6, mapOut)
  }
}

private[storecache] final class Logic7[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, Z](
    l1: Logic1[In, SA, A],
    l2: Logic1[In, SB, B],
    l3: Logic1[In, SC, C],
    l4: Logic1[In, SD, D],
    l5: Logic1[In, SE, E],
    l6: Logic1[In, SF, F],
    l7: Logic1[In, SG, G],
    mapOut: (A,B,C,D,E,F,G) => Z) extends StoreCache.Logic[In, Z] {

  type Self[II, ZZ] = Logic7[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, ZZ]

  override type Cache = StoreCache7[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, Z]

  override def contramap[X](ff: X => In): Self[X, Z] =
    new Logic7(l1.contramap(ff), l2.contramap(ff), l3.contramap(ff), l4.contramap(ff), l5.contramap(ff), l6.contramap(ff), l7.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new Logic7(l1, l2, l3, l4, l5, l6, l7, (a,b,c,d,e,f,g) => ff(mapOut(a,b,c,d,e,f,g)))

  override def init(i: In): Cache = {
    val s1 = l1.init(i)
    val s2 = l2.init(i)
    val s3 = l3.init(i)
    val s4 = l4.init(i)
    val s5 = l5.init(i)
    val s6 = l6.init(i)
    val s7 = l7.init(i)
    new StoreCache7(s1, s2, s3, s4, s5, s6, s7, mapOut)
  }

  override def next(prev: Cache, i: In): Cache = {
    val x1 = l1.next(prev.s1, i)
    val x2 = l2.next(prev.s2, i)
    val x3 = l3.next(prev.s3, i)
    val x4 = l4.next(prev.s4, i)
    val x5 = l5.next(prev.s5, i)
    val x6 = l6.next(prev.s6, i)
    val x7 = l7.next(prev.s7, i)
    if ((x1 eq prev.s1) && (x2 eq prev.s2) && (x3 eq prev.s3) && (x4 eq prev.s4) && (x5 eq prev.s5) && (x6 eq prev.s6) && (x7 eq prev.s7))
      prev
    else
      new StoreCache7(x1, x2, x3, x4, x5, x6, x7, mapOut)
  }
}

private[storecache] final class Logic8[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, Z](
    l1: Logic1[In, SA, A],
    l2: Logic1[In, SB, B],
    l3: Logic1[In, SC, C],
    l4: Logic1[In, SD, D],
    l5: Logic1[In, SE, E],
    l6: Logic1[In, SF, F],
    l7: Logic1[In, SG, G],
    l8: Logic1[In, SH, H],
    mapOut: (A,B,C,D,E,F,G,H) => Z) extends StoreCache.Logic[In, Z] {

  type Self[II, ZZ] = Logic8[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, ZZ]

  override type Cache = StoreCache8[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, Z]

  override def contramap[X](ff: X => In): Self[X, Z] =
    new Logic8(l1.contramap(ff), l2.contramap(ff), l3.contramap(ff), l4.contramap(ff), l5.contramap(ff), l6.contramap(ff), l7.contramap(ff), l8.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new Logic8(l1, l2, l3, l4, l5, l6, l7, l8, (a,b,c,d,e,f,g,h) => ff(mapOut(a,b,c,d,e,f,g,h)))

  override def init(i: In): Cache = {
    val s1 = l1.init(i)
    val s2 = l2.init(i)
    val s3 = l3.init(i)
    val s4 = l4.init(i)
    val s5 = l5.init(i)
    val s6 = l6.init(i)
    val s7 = l7.init(i)
    val s8 = l8.init(i)
    new StoreCache8(s1, s2, s3, s4, s5, s6, s7, s8, mapOut)
  }

  override def next(prev: Cache, i: In): Cache = {
    val x1 = l1.next(prev.s1, i)
    val x2 = l2.next(prev.s2, i)
    val x3 = l3.next(prev.s3, i)
    val x4 = l4.next(prev.s4, i)
    val x5 = l5.next(prev.s5, i)
    val x6 = l6.next(prev.s6, i)
    val x7 = l7.next(prev.s7, i)
    val x8 = l8.next(prev.s8, i)
    if ((x1 eq prev.s1) && (x2 eq prev.s2) && (x3 eq prev.s3) && (x4 eq prev.s4) && (x5 eq prev.s5) && (x6 eq prev.s6) && (x7 eq prev.s7) && (x8 eq prev.s8))
      prev
    else
      new StoreCache8(x1, x2, x3, x4, x5, x6, x7, x8, mapOut)
  }
}

private[storecache] final class Logic9[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, Z](
    l1: Logic1[In, SA, A],
    l2: Logic1[In, SB, B],
    l3: Logic1[In, SC, C],
    l4: Logic1[In, SD, D],
    l5: Logic1[In, SE, E],
    l6: Logic1[In, SF, F],
    l7: Logic1[In, SG, G],
    l8: Logic1[In, SH, H],
    l9: Logic1[In, SI, I],
    mapOut: (A,B,C,D,E,F,G,H,I) => Z) extends StoreCache.Logic[In, Z] {

  type Self[II, ZZ] = Logic9[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, ZZ]

  override type Cache = StoreCache9[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, Z]

  override def contramap[X](ff: X => In): Self[X, Z] =
    new Logic9(l1.contramap(ff), l2.contramap(ff), l3.contramap(ff), l4.contramap(ff), l5.contramap(ff), l6.contramap(ff), l7.contramap(ff), l8.contramap(ff), l9.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new Logic9(l1, l2, l3, l4, l5, l6, l7, l8, l9, (a,b,c,d,e,f,g,h,i) => ff(mapOut(a,b,c,d,e,f,g,h,i)))

  override def init(i: In): Cache = {
    val s1 = l1.init(i)
    val s2 = l2.init(i)
    val s3 = l3.init(i)
    val s4 = l4.init(i)
    val s5 = l5.init(i)
    val s6 = l6.init(i)
    val s7 = l7.init(i)
    val s8 = l8.init(i)
    val s9 = l9.init(i)
    new StoreCache9(s1, s2, s3, s4, s5, s6, s7, s8, s9, mapOut)
  }

  override def next(prev: Cache, i: In): Cache = {
    val x1 = l1.next(prev.s1, i)
    val x2 = l2.next(prev.s2, i)
    val x3 = l3.next(prev.s3, i)
    val x4 = l4.next(prev.s4, i)
    val x5 = l5.next(prev.s5, i)
    val x6 = l6.next(prev.s6, i)
    val x7 = l7.next(prev.s7, i)
    val x8 = l8.next(prev.s8, i)
    val x9 = l9.next(prev.s9, i)
    if ((x1 eq prev.s1) && (x2 eq prev.s2) && (x3 eq prev.s3) && (x4 eq prev.s4) && (x5 eq prev.s5) && (x6 eq prev.s6) && (x7 eq prev.s7) && (x8 eq prev.s8) && (x9 eq prev.s9))
      prev
    else
      new StoreCache9(x1, x2, x3, x4, x5, x6, x7, x8, x9, mapOut)
  }
}

private[storecache] final class Logic10[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, Z](
    l1: Logic1[In, SA, A],
    l2: Logic1[In, SB, B],
    l3: Logic1[In, SC, C],
    l4: Logic1[In, SD, D],
    l5: Logic1[In, SE, E],
    l6: Logic1[In, SF, F],
    l7: Logic1[In, SG, G],
    l8: Logic1[In, SH, H],
    l9: Logic1[In, SI, I],
    l10: Logic1[In, SJ, J],
    mapOut: (A,B,C,D,E,F,G,H,I,J) => Z) extends StoreCache.Logic[In, Z] {

  type Self[II, ZZ] = Logic10[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, ZZ]

  override type Cache = StoreCache10[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, Z]

  override def contramap[X](ff: X => In): Self[X, Z] =
    new Logic10(l1.contramap(ff), l2.contramap(ff), l3.contramap(ff), l4.contramap(ff), l5.contramap(ff), l6.contramap(ff), l7.contramap(ff), l8.contramap(ff), l9.contramap(ff), l10.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new Logic10(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, (a,b,c,d,e,f,g,h,i,j) => ff(mapOut(a,b,c,d,e,f,g,h,i,j)))

  override def init(i: In): Cache = {
    val s1 = l1.init(i)
    val s2 = l2.init(i)
    val s3 = l3.init(i)
    val s4 = l4.init(i)
    val s5 = l5.init(i)
    val s6 = l6.init(i)
    val s7 = l7.init(i)
    val s8 = l8.init(i)
    val s9 = l9.init(i)
    val s10 = l10.init(i)
    new StoreCache10(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, mapOut)
  }

  override def next(prev: Cache, i: In): Cache = {
    val x1 = l1.next(prev.s1, i)
    val x2 = l2.next(prev.s2, i)
    val x3 = l3.next(prev.s3, i)
    val x4 = l4.next(prev.s4, i)
    val x5 = l5.next(prev.s5, i)
    val x6 = l6.next(prev.s6, i)
    val x7 = l7.next(prev.s7, i)
    val x8 = l8.next(prev.s8, i)
    val x9 = l9.next(prev.s9, i)
    val x10 = l10.next(prev.s10, i)
    if ((x1 eq prev.s1) && (x2 eq prev.s2) && (x3 eq prev.s3) && (x4 eq prev.s4) && (x5 eq prev.s5) && (x6 eq prev.s6) && (x7 eq prev.s7) && (x8 eq prev.s8) && (x9 eq prev.s9) && (x10 eq prev.s10))
      prev
    else
      new StoreCache10(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, mapOut)
  }
}

private[storecache] final class Logic11[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, Z](
    l1: Logic1[In, SA, A],
    l2: Logic1[In, SB, B],
    l3: Logic1[In, SC, C],
    l4: Logic1[In, SD, D],
    l5: Logic1[In, SE, E],
    l6: Logic1[In, SF, F],
    l7: Logic1[In, SG, G],
    l8: Logic1[In, SH, H],
    l9: Logic1[In, SI, I],
    l10: Logic1[In, SJ, J],
    l11: Logic1[In, SK, K],
    mapOut: (A,B,C,D,E,F,G,H,I,J,K) => Z) extends StoreCache.Logic[In, Z] {

  type Self[II, ZZ] = Logic11[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, ZZ]

  override type Cache = StoreCache11[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, Z]

  override def contramap[X](ff: X => In): Self[X, Z] =
    new Logic11(l1.contramap(ff), l2.contramap(ff), l3.contramap(ff), l4.contramap(ff), l5.contramap(ff), l6.contramap(ff), l7.contramap(ff), l8.contramap(ff), l9.contramap(ff), l10.contramap(ff), l11.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new Logic11(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, (a,b,c,d,e,f,g,h,i,j,k) => ff(mapOut(a,b,c,d,e,f,g,h,i,j,k)))

  override def init(i: In): Cache = {
    val s1 = l1.init(i)
    val s2 = l2.init(i)
    val s3 = l3.init(i)
    val s4 = l4.init(i)
    val s5 = l5.init(i)
    val s6 = l6.init(i)
    val s7 = l7.init(i)
    val s8 = l8.init(i)
    val s9 = l9.init(i)
    val s10 = l10.init(i)
    val s11 = l11.init(i)
    new StoreCache11(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, mapOut)
  }

  override def next(prev: Cache, i: In): Cache = {
    val x1 = l1.next(prev.s1, i)
    val x2 = l2.next(prev.s2, i)
    val x3 = l3.next(prev.s3, i)
    val x4 = l4.next(prev.s4, i)
    val x5 = l5.next(prev.s5, i)
    val x6 = l6.next(prev.s6, i)
    val x7 = l7.next(prev.s7, i)
    val x8 = l8.next(prev.s8, i)
    val x9 = l9.next(prev.s9, i)
    val x10 = l10.next(prev.s10, i)
    val x11 = l11.next(prev.s11, i)
    if ((x1 eq prev.s1) && (x2 eq prev.s2) && (x3 eq prev.s3) && (x4 eq prev.s4) && (x5 eq prev.s5) && (x6 eq prev.s6) && (x7 eq prev.s7) && (x8 eq prev.s8) && (x9 eq prev.s9) && (x10 eq prev.s10) && (x11 eq prev.s11))
      prev
    else
      new StoreCache11(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, mapOut)
  }
}

private[storecache] final class Logic12[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, Z](
    l1: Logic1[In, SA, A],
    l2: Logic1[In, SB, B],
    l3: Logic1[In, SC, C],
    l4: Logic1[In, SD, D],
    l5: Logic1[In, SE, E],
    l6: Logic1[In, SF, F],
    l7: Logic1[In, SG, G],
    l8: Logic1[In, SH, H],
    l9: Logic1[In, SI, I],
    l10: Logic1[In, SJ, J],
    l11: Logic1[In, SK, K],
    l12: Logic1[In, SL, L],
    mapOut: (A,B,C,D,E,F,G,H,I,J,K,L) => Z) extends StoreCache.Logic[In, Z] {

  type Self[II, ZZ] = Logic12[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, ZZ]

  override type Cache = StoreCache12[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, Z]

  override def contramap[X](ff: X => In): Self[X, Z] =
    new Logic12(l1.contramap(ff), l2.contramap(ff), l3.contramap(ff), l4.contramap(ff), l5.contramap(ff), l6.contramap(ff), l7.contramap(ff), l8.contramap(ff), l9.contramap(ff), l10.contramap(ff), l11.contramap(ff), l12.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new Logic12(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, (a,b,c,d,e,f,g,h,i,j,k,l) => ff(mapOut(a,b,c,d,e,f,g,h,i,j,k,l)))

  override def init(i: In): Cache = {
    val s1 = l1.init(i)
    val s2 = l2.init(i)
    val s3 = l3.init(i)
    val s4 = l4.init(i)
    val s5 = l5.init(i)
    val s6 = l6.init(i)
    val s7 = l7.init(i)
    val s8 = l8.init(i)
    val s9 = l9.init(i)
    val s10 = l10.init(i)
    val s11 = l11.init(i)
    val s12 = l12.init(i)
    new StoreCache12(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, mapOut)
  }

  override def next(prev: Cache, i: In): Cache = {
    val x1 = l1.next(prev.s1, i)
    val x2 = l2.next(prev.s2, i)
    val x3 = l3.next(prev.s3, i)
    val x4 = l4.next(prev.s4, i)
    val x5 = l5.next(prev.s5, i)
    val x6 = l6.next(prev.s6, i)
    val x7 = l7.next(prev.s7, i)
    val x8 = l8.next(prev.s8, i)
    val x9 = l9.next(prev.s9, i)
    val x10 = l10.next(prev.s10, i)
    val x11 = l11.next(prev.s11, i)
    val x12 = l12.next(prev.s12, i)
    if ((x1 eq prev.s1) && (x2 eq prev.s2) && (x3 eq prev.s3) && (x4 eq prev.s4) && (x5 eq prev.s5) && (x6 eq prev.s6) && (x7 eq prev.s7) && (x8 eq prev.s8) && (x9 eq prev.s9) && (x10 eq prev.s10) && (x11 eq prev.s11) && (x12 eq prev.s12))
      prev
    else
      new StoreCache12(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, mapOut)
  }
}

private[storecache] final class Logic13[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, Z](
    l1: Logic1[In, SA, A],
    l2: Logic1[In, SB, B],
    l3: Logic1[In, SC, C],
    l4: Logic1[In, SD, D],
    l5: Logic1[In, SE, E],
    l6: Logic1[In, SF, F],
    l7: Logic1[In, SG, G],
    l8: Logic1[In, SH, H],
    l9: Logic1[In, SI, I],
    l10: Logic1[In, SJ, J],
    l11: Logic1[In, SK, K],
    l12: Logic1[In, SL, L],
    l13: Logic1[In, SM, M],
    mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M) => Z) extends StoreCache.Logic[In, Z] {

  type Self[II, ZZ] = Logic13[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, ZZ]

  override type Cache = StoreCache13[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, Z]

  override def contramap[X](ff: X => In): Self[X, Z] =
    new Logic13(l1.contramap(ff), l2.contramap(ff), l3.contramap(ff), l4.contramap(ff), l5.contramap(ff), l6.contramap(ff), l7.contramap(ff), l8.contramap(ff), l9.contramap(ff), l10.contramap(ff), l11.contramap(ff), l12.contramap(ff), l13.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new Logic13(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, (a,b,c,d,e,f,g,h,i,j,k,l,m) => ff(mapOut(a,b,c,d,e,f,g,h,i,j,k,l,m)))

  override def init(i: In): Cache = {
    val s1 = l1.init(i)
    val s2 = l2.init(i)
    val s3 = l3.init(i)
    val s4 = l4.init(i)
    val s5 = l5.init(i)
    val s6 = l6.init(i)
    val s7 = l7.init(i)
    val s8 = l8.init(i)
    val s9 = l9.init(i)
    val s10 = l10.init(i)
    val s11 = l11.init(i)
    val s12 = l12.init(i)
    val s13 = l13.init(i)
    new StoreCache13(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, mapOut)
  }

  override def next(prev: Cache, i: In): Cache = {
    val x1 = l1.next(prev.s1, i)
    val x2 = l2.next(prev.s2, i)
    val x3 = l3.next(prev.s3, i)
    val x4 = l4.next(prev.s4, i)
    val x5 = l5.next(prev.s5, i)
    val x6 = l6.next(prev.s6, i)
    val x7 = l7.next(prev.s7, i)
    val x8 = l8.next(prev.s8, i)
    val x9 = l9.next(prev.s9, i)
    val x10 = l10.next(prev.s10, i)
    val x11 = l11.next(prev.s11, i)
    val x12 = l12.next(prev.s12, i)
    val x13 = l13.next(prev.s13, i)
    if ((x1 eq prev.s1) && (x2 eq prev.s2) && (x3 eq prev.s3) && (x4 eq prev.s4) && (x5 eq prev.s5) && (x6 eq prev.s6) && (x7 eq prev.s7) && (x8 eq prev.s8) && (x9 eq prev.s9) && (x10 eq prev.s10) && (x11 eq prev.s11) && (x12 eq prev.s12) && (x13 eq prev.s13))
      prev
    else
      new StoreCache13(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, mapOut)
  }
}

private[storecache] final class Logic14[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, Z](
    l1: Logic1[In, SA, A],
    l2: Logic1[In, SB, B],
    l3: Logic1[In, SC, C],
    l4: Logic1[In, SD, D],
    l5: Logic1[In, SE, E],
    l6: Logic1[In, SF, F],
    l7: Logic1[In, SG, G],
    l8: Logic1[In, SH, H],
    l9: Logic1[In, SI, I],
    l10: Logic1[In, SJ, J],
    l11: Logic1[In, SK, K],
    l12: Logic1[In, SL, L],
    l13: Logic1[In, SM, M],
    l14: Logic1[In, SN, N],
    mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N) => Z) extends StoreCache.Logic[In, Z] {

  type Self[II, ZZ] = Logic14[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, ZZ]

  override type Cache = StoreCache14[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, Z]

  override def contramap[X](ff: X => In): Self[X, Z] =
    new Logic14(l1.contramap(ff), l2.contramap(ff), l3.contramap(ff), l4.contramap(ff), l5.contramap(ff), l6.contramap(ff), l7.contramap(ff), l8.contramap(ff), l9.contramap(ff), l10.contramap(ff), l11.contramap(ff), l12.contramap(ff), l13.contramap(ff), l14.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new Logic14(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, (a,b,c,d,e,f,g,h,i,j,k,l,m,n) => ff(mapOut(a,b,c,d,e,f,g,h,i,j,k,l,m,n)))

  override def init(i: In): Cache = {
    val s1 = l1.init(i)
    val s2 = l2.init(i)
    val s3 = l3.init(i)
    val s4 = l4.init(i)
    val s5 = l5.init(i)
    val s6 = l6.init(i)
    val s7 = l7.init(i)
    val s8 = l8.init(i)
    val s9 = l9.init(i)
    val s10 = l10.init(i)
    val s11 = l11.init(i)
    val s12 = l12.init(i)
    val s13 = l13.init(i)
    val s14 = l14.init(i)
    new StoreCache14(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, mapOut)
  }

  override def next(prev: Cache, i: In): Cache = {
    val x1 = l1.next(prev.s1, i)
    val x2 = l2.next(prev.s2, i)
    val x3 = l3.next(prev.s3, i)
    val x4 = l4.next(prev.s4, i)
    val x5 = l5.next(prev.s5, i)
    val x6 = l6.next(prev.s6, i)
    val x7 = l7.next(prev.s7, i)
    val x8 = l8.next(prev.s8, i)
    val x9 = l9.next(prev.s9, i)
    val x10 = l10.next(prev.s10, i)
    val x11 = l11.next(prev.s11, i)
    val x12 = l12.next(prev.s12, i)
    val x13 = l13.next(prev.s13, i)
    val x14 = l14.next(prev.s14, i)
    if ((x1 eq prev.s1) && (x2 eq prev.s2) && (x3 eq prev.s3) && (x4 eq prev.s4) && (x5 eq prev.s5) && (x6 eq prev.s6) && (x7 eq prev.s7) && (x8 eq prev.s8) && (x9 eq prev.s9) && (x10 eq prev.s10) && (x11 eq prev.s11) && (x12 eq prev.s12) && (x13 eq prev.s13) && (x14 eq prev.s14))
      prev
    else
      new StoreCache14(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, mapOut)
  }
}

private[storecache] final class Logic15[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, Z](
    l1: Logic1[In, SA, A],
    l2: Logic1[In, SB, B],
    l3: Logic1[In, SC, C],
    l4: Logic1[In, SD, D],
    l5: Logic1[In, SE, E],
    l6: Logic1[In, SF, F],
    l7: Logic1[In, SG, G],
    l8: Logic1[In, SH, H],
    l9: Logic1[In, SI, I],
    l10: Logic1[In, SJ, J],
    l11: Logic1[In, SK, K],
    l12: Logic1[In, SL, L],
    l13: Logic1[In, SM, M],
    l14: Logic1[In, SN, N],
    l15: Logic1[In, SO, O],
    mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) => Z) extends StoreCache.Logic[In, Z] {

  type Self[II, ZZ] = Logic15[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, ZZ]

  override type Cache = StoreCache15[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, Z]

  override def contramap[X](ff: X => In): Self[X, Z] =
    new Logic15(l1.contramap(ff), l2.contramap(ff), l3.contramap(ff), l4.contramap(ff), l5.contramap(ff), l6.contramap(ff), l7.contramap(ff), l8.contramap(ff), l9.contramap(ff), l10.contramap(ff), l11.contramap(ff), l12.contramap(ff), l13.contramap(ff), l14.contramap(ff), l15.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new Logic15(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) => ff(mapOut(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)))

  override def init(i: In): Cache = {
    val s1 = l1.init(i)
    val s2 = l2.init(i)
    val s3 = l3.init(i)
    val s4 = l4.init(i)
    val s5 = l5.init(i)
    val s6 = l6.init(i)
    val s7 = l7.init(i)
    val s8 = l8.init(i)
    val s9 = l9.init(i)
    val s10 = l10.init(i)
    val s11 = l11.init(i)
    val s12 = l12.init(i)
    val s13 = l13.init(i)
    val s14 = l14.init(i)
    val s15 = l15.init(i)
    new StoreCache15(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, mapOut)
  }

  override def next(prev: Cache, i: In): Cache = {
    val x1 = l1.next(prev.s1, i)
    val x2 = l2.next(prev.s2, i)
    val x3 = l3.next(prev.s3, i)
    val x4 = l4.next(prev.s4, i)
    val x5 = l5.next(prev.s5, i)
    val x6 = l6.next(prev.s6, i)
    val x7 = l7.next(prev.s7, i)
    val x8 = l8.next(prev.s8, i)
    val x9 = l9.next(prev.s9, i)
    val x10 = l10.next(prev.s10, i)
    val x11 = l11.next(prev.s11, i)
    val x12 = l12.next(prev.s12, i)
    val x13 = l13.next(prev.s13, i)
    val x14 = l14.next(prev.s14, i)
    val x15 = l15.next(prev.s15, i)
    if ((x1 eq prev.s1) && (x2 eq prev.s2) && (x3 eq prev.s3) && (x4 eq prev.s4) && (x5 eq prev.s5) && (x6 eq prev.s6) && (x7 eq prev.s7) && (x8 eq prev.s8) && (x9 eq prev.s9) && (x10 eq prev.s10) && (x11 eq prev.s11) && (x12 eq prev.s12) && (x13 eq prev.s13) && (x14 eq prev.s14) && (x15 eq prev.s15))
      prev
    else
      new StoreCache15(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, mapOut)
  }
}

private[storecache] final class Logic16[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, Z](
    l1: Logic1[In, SA, A],
    l2: Logic1[In, SB, B],
    l3: Logic1[In, SC, C],
    l4: Logic1[In, SD, D],
    l5: Logic1[In, SE, E],
    l6: Logic1[In, SF, F],
    l7: Logic1[In, SG, G],
    l8: Logic1[In, SH, H],
    l9: Logic1[In, SI, I],
    l10: Logic1[In, SJ, J],
    l11: Logic1[In, SK, K],
    l12: Logic1[In, SL, L],
    l13: Logic1[In, SM, M],
    l14: Logic1[In, SN, N],
    l15: Logic1[In, SO, O],
    l16: Logic1[In, SP, P],
    mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) => Z) extends StoreCache.Logic[In, Z] {

  type Self[II, ZZ] = Logic16[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, ZZ]

  override type Cache = StoreCache16[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, Z]

  override def contramap[X](ff: X => In): Self[X, Z] =
    new Logic16(l1.contramap(ff), l2.contramap(ff), l3.contramap(ff), l4.contramap(ff), l5.contramap(ff), l6.contramap(ff), l7.contramap(ff), l8.contramap(ff), l9.contramap(ff), l10.contramap(ff), l11.contramap(ff), l12.contramap(ff), l13.contramap(ff), l14.contramap(ff), l15.contramap(ff), l16.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new Logic16(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16, (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) => ff(mapOut(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)))

  override def init(i: In): Cache = {
    val s1 = l1.init(i)
    val s2 = l2.init(i)
    val s3 = l3.init(i)
    val s4 = l4.init(i)
    val s5 = l5.init(i)
    val s6 = l6.init(i)
    val s7 = l7.init(i)
    val s8 = l8.init(i)
    val s9 = l9.init(i)
    val s10 = l10.init(i)
    val s11 = l11.init(i)
    val s12 = l12.init(i)
    val s13 = l13.init(i)
    val s14 = l14.init(i)
    val s15 = l15.init(i)
    val s16 = l16.init(i)
    new StoreCache16(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, mapOut)
  }

  override def next(prev: Cache, i: In): Cache = {
    val x1 = l1.next(prev.s1, i)
    val x2 = l2.next(prev.s2, i)
    val x3 = l3.next(prev.s3, i)
    val x4 = l4.next(prev.s4, i)
    val x5 = l5.next(prev.s5, i)
    val x6 = l6.next(prev.s6, i)
    val x7 = l7.next(prev.s7, i)
    val x8 = l8.next(prev.s8, i)
    val x9 = l9.next(prev.s9, i)
    val x10 = l10.next(prev.s10, i)
    val x11 = l11.next(prev.s11, i)
    val x12 = l12.next(prev.s12, i)
    val x13 = l13.next(prev.s13, i)
    val x14 = l14.next(prev.s14, i)
    val x15 = l15.next(prev.s15, i)
    val x16 = l16.next(prev.s16, i)
    if ((x1 eq prev.s1) && (x2 eq prev.s2) && (x3 eq prev.s3) && (x4 eq prev.s4) && (x5 eq prev.s5) && (x6 eq prev.s6) && (x7 eq prev.s7) && (x8 eq prev.s8) && (x9 eq prev.s9) && (x10 eq prev.s10) && (x11 eq prev.s11) && (x12 eq prev.s12) && (x13 eq prev.s13) && (x14 eq prev.s14) && (x15 eq prev.s15) && (x16 eq prev.s16))
      prev
    else
      new StoreCache16(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, mapOut)
  }
}

private[storecache] final class Logic17[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, Z](
    l1: Logic1[In, SA, A],
    l2: Logic1[In, SB, B],
    l3: Logic1[In, SC, C],
    l4: Logic1[In, SD, D],
    l5: Logic1[In, SE, E],
    l6: Logic1[In, SF, F],
    l7: Logic1[In, SG, G],
    l8: Logic1[In, SH, H],
    l9: Logic1[In, SI, I],
    l10: Logic1[In, SJ, J],
    l11: Logic1[In, SK, K],
    l12: Logic1[In, SL, L],
    l13: Logic1[In, SM, M],
    l14: Logic1[In, SN, N],
    l15: Logic1[In, SO, O],
    l16: Logic1[In, SP, P],
    l17: Logic1[In, SQ, Q],
    mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) => Z) extends StoreCache.Logic[In, Z] {

  type Self[II, ZZ] = Logic17[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, ZZ]

  override type Cache = StoreCache17[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, Z]

  override def contramap[X](ff: X => In): Self[X, Z] =
    new Logic17(l1.contramap(ff), l2.contramap(ff), l3.contramap(ff), l4.contramap(ff), l5.contramap(ff), l6.contramap(ff), l7.contramap(ff), l8.contramap(ff), l9.contramap(ff), l10.contramap(ff), l11.contramap(ff), l12.contramap(ff), l13.contramap(ff), l14.contramap(ff), l15.contramap(ff), l16.contramap(ff), l17.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new Logic17(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16, l17, (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) => ff(mapOut(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)))

  override def init(i: In): Cache = {
    val s1 = l1.init(i)
    val s2 = l2.init(i)
    val s3 = l3.init(i)
    val s4 = l4.init(i)
    val s5 = l5.init(i)
    val s6 = l6.init(i)
    val s7 = l7.init(i)
    val s8 = l8.init(i)
    val s9 = l9.init(i)
    val s10 = l10.init(i)
    val s11 = l11.init(i)
    val s12 = l12.init(i)
    val s13 = l13.init(i)
    val s14 = l14.init(i)
    val s15 = l15.init(i)
    val s16 = l16.init(i)
    val s17 = l17.init(i)
    new StoreCache17(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, mapOut)
  }

  override def next(prev: Cache, i: In): Cache = {
    val x1 = l1.next(prev.s1, i)
    val x2 = l2.next(prev.s2, i)
    val x3 = l3.next(prev.s3, i)
    val x4 = l4.next(prev.s4, i)
    val x5 = l5.next(prev.s5, i)
    val x6 = l6.next(prev.s6, i)
    val x7 = l7.next(prev.s7, i)
    val x8 = l8.next(prev.s8, i)
    val x9 = l9.next(prev.s9, i)
    val x10 = l10.next(prev.s10, i)
    val x11 = l11.next(prev.s11, i)
    val x12 = l12.next(prev.s12, i)
    val x13 = l13.next(prev.s13, i)
    val x14 = l14.next(prev.s14, i)
    val x15 = l15.next(prev.s15, i)
    val x16 = l16.next(prev.s16, i)
    val x17 = l17.next(prev.s17, i)
    if ((x1 eq prev.s1) && (x2 eq prev.s2) && (x3 eq prev.s3) && (x4 eq prev.s4) && (x5 eq prev.s5) && (x6 eq prev.s6) && (x7 eq prev.s7) && (x8 eq prev.s8) && (x9 eq prev.s9) && (x10 eq prev.s10) && (x11 eq prev.s11) && (x12 eq prev.s12) && (x13 eq prev.s13) && (x14 eq prev.s14) && (x15 eq prev.s15) && (x16 eq prev.s16) && (x17 eq prev.s17))
      prev
    else
      new StoreCache17(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, mapOut)
  }
}

private[storecache] final class Logic18[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, Z](
    l1: Logic1[In, SA, A],
    l2: Logic1[In, SB, B],
    l3: Logic1[In, SC, C],
    l4: Logic1[In, SD, D],
    l5: Logic1[In, SE, E],
    l6: Logic1[In, SF, F],
    l7: Logic1[In, SG, G],
    l8: Logic1[In, SH, H],
    l9: Logic1[In, SI, I],
    l10: Logic1[In, SJ, J],
    l11: Logic1[In, SK, K],
    l12: Logic1[In, SL, L],
    l13: Logic1[In, SM, M],
    l14: Logic1[In, SN, N],
    l15: Logic1[In, SO, O],
    l16: Logic1[In, SP, P],
    l17: Logic1[In, SQ, Q],
    l18: Logic1[In, SR, R],
    mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) => Z) extends StoreCache.Logic[In, Z] {

  type Self[II, ZZ] = Logic18[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, ZZ]

  override type Cache = StoreCache18[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, Z]

  override def contramap[X](ff: X => In): Self[X, Z] =
    new Logic18(l1.contramap(ff), l2.contramap(ff), l3.contramap(ff), l4.contramap(ff), l5.contramap(ff), l6.contramap(ff), l7.contramap(ff), l8.contramap(ff), l9.contramap(ff), l10.contramap(ff), l11.contramap(ff), l12.contramap(ff), l13.contramap(ff), l14.contramap(ff), l15.contramap(ff), l16.contramap(ff), l17.contramap(ff), l18.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new Logic18(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16, l17, l18, (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) => ff(mapOut(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)))

  override def init(i: In): Cache = {
    val s1 = l1.init(i)
    val s2 = l2.init(i)
    val s3 = l3.init(i)
    val s4 = l4.init(i)
    val s5 = l5.init(i)
    val s6 = l6.init(i)
    val s7 = l7.init(i)
    val s8 = l8.init(i)
    val s9 = l9.init(i)
    val s10 = l10.init(i)
    val s11 = l11.init(i)
    val s12 = l12.init(i)
    val s13 = l13.init(i)
    val s14 = l14.init(i)
    val s15 = l15.init(i)
    val s16 = l16.init(i)
    val s17 = l17.init(i)
    val s18 = l18.init(i)
    new StoreCache18(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, mapOut)
  }

  override def next(prev: Cache, i: In): Cache = {
    val x1 = l1.next(prev.s1, i)
    val x2 = l2.next(prev.s2, i)
    val x3 = l3.next(prev.s3, i)
    val x4 = l4.next(prev.s4, i)
    val x5 = l5.next(prev.s5, i)
    val x6 = l6.next(prev.s6, i)
    val x7 = l7.next(prev.s7, i)
    val x8 = l8.next(prev.s8, i)
    val x9 = l9.next(prev.s9, i)
    val x10 = l10.next(prev.s10, i)
    val x11 = l11.next(prev.s11, i)
    val x12 = l12.next(prev.s12, i)
    val x13 = l13.next(prev.s13, i)
    val x14 = l14.next(prev.s14, i)
    val x15 = l15.next(prev.s15, i)
    val x16 = l16.next(prev.s16, i)
    val x17 = l17.next(prev.s17, i)
    val x18 = l18.next(prev.s18, i)
    if ((x1 eq prev.s1) && (x2 eq prev.s2) && (x3 eq prev.s3) && (x4 eq prev.s4) && (x5 eq prev.s5) && (x6 eq prev.s6) && (x7 eq prev.s7) && (x8 eq prev.s8) && (x9 eq prev.s9) && (x10 eq prev.s10) && (x11 eq prev.s11) && (x12 eq prev.s12) && (x13 eq prev.s13) && (x14 eq prev.s14) && (x15 eq prev.s15) && (x16 eq prev.s16) && (x17 eq prev.s17) && (x18 eq prev.s18))
      prev
    else
      new StoreCache18(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, mapOut)
  }
}

private[storecache] final class Logic19[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, Z](
    l1: Logic1[In, SA, A],
    l2: Logic1[In, SB, B],
    l3: Logic1[In, SC, C],
    l4: Logic1[In, SD, D],
    l5: Logic1[In, SE, E],
    l6: Logic1[In, SF, F],
    l7: Logic1[In, SG, G],
    l8: Logic1[In, SH, H],
    l9: Logic1[In, SI, I],
    l10: Logic1[In, SJ, J],
    l11: Logic1[In, SK, K],
    l12: Logic1[In, SL, L],
    l13: Logic1[In, SM, M],
    l14: Logic1[In, SN, N],
    l15: Logic1[In, SO, O],
    l16: Logic1[In, SP, P],
    l17: Logic1[In, SQ, Q],
    l18: Logic1[In, SR, R],
    l19: Logic1[In, SS, S],
    mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) => Z) extends StoreCache.Logic[In, Z] {

  type Self[II, ZZ] = Logic19[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, ZZ]

  override type Cache = StoreCache19[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, Z]

  override def contramap[X](ff: X => In): Self[X, Z] =
    new Logic19(l1.contramap(ff), l2.contramap(ff), l3.contramap(ff), l4.contramap(ff), l5.contramap(ff), l6.contramap(ff), l7.contramap(ff), l8.contramap(ff), l9.contramap(ff), l10.contramap(ff), l11.contramap(ff), l12.contramap(ff), l13.contramap(ff), l14.contramap(ff), l15.contramap(ff), l16.contramap(ff), l17.contramap(ff), l18.contramap(ff), l19.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new Logic19(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16, l17, l18, l19, (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) => ff(mapOut(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)))

  override def init(i: In): Cache = {
    val s1 = l1.init(i)
    val s2 = l2.init(i)
    val s3 = l3.init(i)
    val s4 = l4.init(i)
    val s5 = l5.init(i)
    val s6 = l6.init(i)
    val s7 = l7.init(i)
    val s8 = l8.init(i)
    val s9 = l9.init(i)
    val s10 = l10.init(i)
    val s11 = l11.init(i)
    val s12 = l12.init(i)
    val s13 = l13.init(i)
    val s14 = l14.init(i)
    val s15 = l15.init(i)
    val s16 = l16.init(i)
    val s17 = l17.init(i)
    val s18 = l18.init(i)
    val s19 = l19.init(i)
    new StoreCache19(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, mapOut)
  }

  override def next(prev: Cache, i: In): Cache = {
    val x1 = l1.next(prev.s1, i)
    val x2 = l2.next(prev.s2, i)
    val x3 = l3.next(prev.s3, i)
    val x4 = l4.next(prev.s4, i)
    val x5 = l5.next(prev.s5, i)
    val x6 = l6.next(prev.s6, i)
    val x7 = l7.next(prev.s7, i)
    val x8 = l8.next(prev.s8, i)
    val x9 = l9.next(prev.s9, i)
    val x10 = l10.next(prev.s10, i)
    val x11 = l11.next(prev.s11, i)
    val x12 = l12.next(prev.s12, i)
    val x13 = l13.next(prev.s13, i)
    val x14 = l14.next(prev.s14, i)
    val x15 = l15.next(prev.s15, i)
    val x16 = l16.next(prev.s16, i)
    val x17 = l17.next(prev.s17, i)
    val x18 = l18.next(prev.s18, i)
    val x19 = l19.next(prev.s19, i)
    if ((x1 eq prev.s1) && (x2 eq prev.s2) && (x3 eq prev.s3) && (x4 eq prev.s4) && (x5 eq prev.s5) && (x6 eq prev.s6) && (x7 eq prev.s7) && (x8 eq prev.s8) && (x9 eq prev.s9) && (x10 eq prev.s10) && (x11 eq prev.s11) && (x12 eq prev.s12) && (x13 eq prev.s13) && (x14 eq prev.s14) && (x15 eq prev.s15) && (x16 eq prev.s16) && (x17 eq prev.s17) && (x18 eq prev.s18) && (x19 eq prev.s19))
      prev
    else
      new StoreCache19(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, mapOut)
  }
}

private[storecache] final class Logic20[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, ST,T, Z](
    l1: Logic1[In, SA, A],
    l2: Logic1[In, SB, B],
    l3: Logic1[In, SC, C],
    l4: Logic1[In, SD, D],
    l5: Logic1[In, SE, E],
    l6: Logic1[In, SF, F],
    l7: Logic1[In, SG, G],
    l8: Logic1[In, SH, H],
    l9: Logic1[In, SI, I],
    l10: Logic1[In, SJ, J],
    l11: Logic1[In, SK, K],
    l12: Logic1[In, SL, L],
    l13: Logic1[In, SM, M],
    l14: Logic1[In, SN, N],
    l15: Logic1[In, SO, O],
    l16: Logic1[In, SP, P],
    l17: Logic1[In, SQ, Q],
    l18: Logic1[In, SR, R],
    l19: Logic1[In, SS, S],
    l20: Logic1[In, ST, T],
    mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) => Z) extends StoreCache.Logic[In, Z] {

  type Self[II, ZZ] = Logic20[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, ST,T, ZZ]

  override type Cache = StoreCache20[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, ST,T, Z]

  override def contramap[X](ff: X => In): Self[X, Z] =
    new Logic20(l1.contramap(ff), l2.contramap(ff), l3.contramap(ff), l4.contramap(ff), l5.contramap(ff), l6.contramap(ff), l7.contramap(ff), l8.contramap(ff), l9.contramap(ff), l10.contramap(ff), l11.contramap(ff), l12.contramap(ff), l13.contramap(ff), l14.contramap(ff), l15.contramap(ff), l16.contramap(ff), l17.contramap(ff), l18.contramap(ff), l19.contramap(ff), l20.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new Logic20(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16, l17, l18, l19, l20, (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) => ff(mapOut(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)))

  override def init(i: In): Cache = {
    val s1 = l1.init(i)
    val s2 = l2.init(i)
    val s3 = l3.init(i)
    val s4 = l4.init(i)
    val s5 = l5.init(i)
    val s6 = l6.init(i)
    val s7 = l7.init(i)
    val s8 = l8.init(i)
    val s9 = l9.init(i)
    val s10 = l10.init(i)
    val s11 = l11.init(i)
    val s12 = l12.init(i)
    val s13 = l13.init(i)
    val s14 = l14.init(i)
    val s15 = l15.init(i)
    val s16 = l16.init(i)
    val s17 = l17.init(i)
    val s18 = l18.init(i)
    val s19 = l19.init(i)
    val s20 = l20.init(i)
    new StoreCache20(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, mapOut)
  }

  override def next(prev: Cache, i: In): Cache = {
    val x1 = l1.next(prev.s1, i)
    val x2 = l2.next(prev.s2, i)
    val x3 = l3.next(prev.s3, i)
    val x4 = l4.next(prev.s4, i)
    val x5 = l5.next(prev.s5, i)
    val x6 = l6.next(prev.s6, i)
    val x7 = l7.next(prev.s7, i)
    val x8 = l8.next(prev.s8, i)
    val x9 = l9.next(prev.s9, i)
    val x10 = l10.next(prev.s10, i)
    val x11 = l11.next(prev.s11, i)
    val x12 = l12.next(prev.s12, i)
    val x13 = l13.next(prev.s13, i)
    val x14 = l14.next(prev.s14, i)
    val x15 = l15.next(prev.s15, i)
    val x16 = l16.next(prev.s16, i)
    val x17 = l17.next(prev.s17, i)
    val x18 = l18.next(prev.s18, i)
    val x19 = l19.next(prev.s19, i)
    val x20 = l20.next(prev.s20, i)
    if ((x1 eq prev.s1) && (x2 eq prev.s2) && (x3 eq prev.s3) && (x4 eq prev.s4) && (x5 eq prev.s5) && (x6 eq prev.s6) && (x7 eq prev.s7) && (x8 eq prev.s8) && (x9 eq prev.s9) && (x10 eq prev.s10) && (x11 eq prev.s11) && (x12 eq prev.s12) && (x13 eq prev.s13) && (x14 eq prev.s14) && (x15 eq prev.s15) && (x16 eq prev.s16) && (x17 eq prev.s17) && (x18 eq prev.s18) && (x19 eq prev.s19) && (x20 eq prev.s20))
      prev
    else
      new StoreCache20(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, mapOut)
  }
}

private[storecache] final class Logic21[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, ST,T, SU,U, Z](
    l1: Logic1[In, SA, A],
    l2: Logic1[In, SB, B],
    l3: Logic1[In, SC, C],
    l4: Logic1[In, SD, D],
    l5: Logic1[In, SE, E],
    l6: Logic1[In, SF, F],
    l7: Logic1[In, SG, G],
    l8: Logic1[In, SH, H],
    l9: Logic1[In, SI, I],
    l10: Logic1[In, SJ, J],
    l11: Logic1[In, SK, K],
    l12: Logic1[In, SL, L],
    l13: Logic1[In, SM, M],
    l14: Logic1[In, SN, N],
    l15: Logic1[In, SO, O],
    l16: Logic1[In, SP, P],
    l17: Logic1[In, SQ, Q],
    l18: Logic1[In, SR, R],
    l19: Logic1[In, SS, S],
    l20: Logic1[In, ST, T],
    l21: Logic1[In, SU, U],
    mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) => Z) extends StoreCache.Logic[In, Z] {

  type Self[II, ZZ] = Logic21[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, ST,T, SU,U, ZZ]

  override type Cache = StoreCache21[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, ST,T, SU,U, Z]

  override def contramap[X](ff: X => In): Self[X, Z] =
    new Logic21(l1.contramap(ff), l2.contramap(ff), l3.contramap(ff), l4.contramap(ff), l5.contramap(ff), l6.contramap(ff), l7.contramap(ff), l8.contramap(ff), l9.contramap(ff), l10.contramap(ff), l11.contramap(ff), l12.contramap(ff), l13.contramap(ff), l14.contramap(ff), l15.contramap(ff), l16.contramap(ff), l17.contramap(ff), l18.contramap(ff), l19.contramap(ff), l20.contramap(ff), l21.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new Logic21(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16, l17, l18, l19, l20, l21, (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) => ff(mapOut(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)))

  override def init(i: In): Cache = {
    val s1 = l1.init(i)
    val s2 = l2.init(i)
    val s3 = l3.init(i)
    val s4 = l4.init(i)
    val s5 = l5.init(i)
    val s6 = l6.init(i)
    val s7 = l7.init(i)
    val s8 = l8.init(i)
    val s9 = l9.init(i)
    val s10 = l10.init(i)
    val s11 = l11.init(i)
    val s12 = l12.init(i)
    val s13 = l13.init(i)
    val s14 = l14.init(i)
    val s15 = l15.init(i)
    val s16 = l16.init(i)
    val s17 = l17.init(i)
    val s18 = l18.init(i)
    val s19 = l19.init(i)
    val s20 = l20.init(i)
    val s21 = l21.init(i)
    new StoreCache21(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, mapOut)
  }

  override def next(prev: Cache, i: In): Cache = {
    val x1 = l1.next(prev.s1, i)
    val x2 = l2.next(prev.s2, i)
    val x3 = l3.next(prev.s3, i)
    val x4 = l4.next(prev.s4, i)
    val x5 = l5.next(prev.s5, i)
    val x6 = l6.next(prev.s6, i)
    val x7 = l7.next(prev.s7, i)
    val x8 = l8.next(prev.s8, i)
    val x9 = l9.next(prev.s9, i)
    val x10 = l10.next(prev.s10, i)
    val x11 = l11.next(prev.s11, i)
    val x12 = l12.next(prev.s12, i)
    val x13 = l13.next(prev.s13, i)
    val x14 = l14.next(prev.s14, i)
    val x15 = l15.next(prev.s15, i)
    val x16 = l16.next(prev.s16, i)
    val x17 = l17.next(prev.s17, i)
    val x18 = l18.next(prev.s18, i)
    val x19 = l19.next(prev.s19, i)
    val x20 = l20.next(prev.s20, i)
    val x21 = l21.next(prev.s21, i)
    if ((x1 eq prev.s1) && (x2 eq prev.s2) && (x3 eq prev.s3) && (x4 eq prev.s4) && (x5 eq prev.s5) && (x6 eq prev.s6) && (x7 eq prev.s7) && (x8 eq prev.s8) && (x9 eq prev.s9) && (x10 eq prev.s10) && (x11 eq prev.s11) && (x12 eq prev.s12) && (x13 eq prev.s13) && (x14 eq prev.s14) && (x15 eq prev.s15) && (x16 eq prev.s16) && (x17 eq prev.s17) && (x18 eq prev.s18) && (x19 eq prev.s19) && (x20 eq prev.s20) && (x21 eq prev.s21))
      prev
    else
      new StoreCache21(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, mapOut)
  }
}

private[storecache] final class Logic22[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, ST,T, SU,U, SV,V, Z](
    l1: Logic1[In, SA, A],
    l2: Logic1[In, SB, B],
    l3: Logic1[In, SC, C],
    l4: Logic1[In, SD, D],
    l5: Logic1[In, SE, E],
    l6: Logic1[In, SF, F],
    l7: Logic1[In, SG, G],
    l8: Logic1[In, SH, H],
    l9: Logic1[In, SI, I],
    l10: Logic1[In, SJ, J],
    l11: Logic1[In, SK, K],
    l12: Logic1[In, SL, L],
    l13: Logic1[In, SM, M],
    l14: Logic1[In, SN, N],
    l15: Logic1[In, SO, O],
    l16: Logic1[In, SP, P],
    l17: Logic1[In, SQ, Q],
    l18: Logic1[In, SR, R],
    l19: Logic1[In, SS, S],
    l20: Logic1[In, ST, T],
    l21: Logic1[In, SU, U],
    l22: Logic1[In, SV, V],
    mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) => Z) extends StoreCache.Logic[In, Z] {

  type Self[II, ZZ] = Logic22[II, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, ST,T, SU,U, SV,V, ZZ]

  override type Cache = StoreCache22[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, ST,T, SU,U, SV,V, Z]

  override def contramap[X](ff: X => In): Self[X, Z] =
    new Logic22(l1.contramap(ff), l2.contramap(ff), l3.contramap(ff), l4.contramap(ff), l5.contramap(ff), l6.contramap(ff), l7.contramap(ff), l8.contramap(ff), l9.contramap(ff), l10.contramap(ff), l11.contramap(ff), l12.contramap(ff), l13.contramap(ff), l14.contramap(ff), l15.contramap(ff), l16.contramap(ff), l17.contramap(ff), l18.contramap(ff), l19.contramap(ff), l20.contramap(ff), l21.contramap(ff), l22.contramap(ff), mapOut)

  override def map[X](ff: Z => X): Self[In, X] =
    new Logic22(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16, l17, l18, l19, l20, l21, l22, (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) => ff(mapOut(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v)))

  override def init(i: In): Cache = {
    val s1 = l1.init(i)
    val s2 = l2.init(i)
    val s3 = l3.init(i)
    val s4 = l4.init(i)
    val s5 = l5.init(i)
    val s6 = l6.init(i)
    val s7 = l7.init(i)
    val s8 = l8.init(i)
    val s9 = l9.init(i)
    val s10 = l10.init(i)
    val s11 = l11.init(i)
    val s12 = l12.init(i)
    val s13 = l13.init(i)
    val s14 = l14.init(i)
    val s15 = l15.init(i)
    val s16 = l16.init(i)
    val s17 = l17.init(i)
    val s18 = l18.init(i)
    val s19 = l19.init(i)
    val s20 = l20.init(i)
    val s21 = l21.init(i)
    val s22 = l22.init(i)
    new StoreCache22(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, mapOut)
  }

  override def next(prev: Cache, i: In): Cache = {
    val x1 = l1.next(prev.s1, i)
    val x2 = l2.next(prev.s2, i)
    val x3 = l3.next(prev.s3, i)
    val x4 = l4.next(prev.s4, i)
    val x5 = l5.next(prev.s5, i)
    val x6 = l6.next(prev.s6, i)
    val x7 = l7.next(prev.s7, i)
    val x8 = l8.next(prev.s8, i)
    val x9 = l9.next(prev.s9, i)
    val x10 = l10.next(prev.s10, i)
    val x11 = l11.next(prev.s11, i)
    val x12 = l12.next(prev.s12, i)
    val x13 = l13.next(prev.s13, i)
    val x14 = l14.next(prev.s14, i)
    val x15 = l15.next(prev.s15, i)
    val x16 = l16.next(prev.s16, i)
    val x17 = l17.next(prev.s17, i)
    val x18 = l18.next(prev.s18, i)
    val x19 = l19.next(prev.s19, i)
    val x20 = l20.next(prev.s20, i)
    val x21 = l21.next(prev.s21, i)
    val x22 = l22.next(prev.s22, i)
    if ((x1 eq prev.s1) && (x2 eq prev.s2) && (x3 eq prev.s3) && (x4 eq prev.s4) && (x5 eq prev.s5) && (x6 eq prev.s6) && (x7 eq prev.s7) && (x8 eq prev.s8) && (x9 eq prev.s9) && (x10 eq prev.s10) && (x11 eq prev.s11) && (x12 eq prev.s12) && (x13 eq prev.s13) && (x14 eq prev.s14) && (x15 eq prev.s15) && (x16 eq prev.s16) && (x17 eq prev.s17) && (x18 eq prev.s18) && (x19 eq prev.s19) && (x20 eq prev.s20) && (x21 eq prev.s21) && (x22 eq prev.s22))
      prev
    else
      new StoreCache22(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, mapOut)
  }
}

abstract class StoreCacheLogicBoilerplate private[storecache]() {

  final def apply2[In, SA,A, SB,B, Z](
      l1: Logic1[In, SA, A],
      l2: Logic1[In, SB, B])(
      mapOut: (A,B) => Z): StoreCache.Logic[In, Z] =
    new Logic2(l1, l2, mapOut)

  final def apply3[In, SA,A, SB,B, SC,C, Z](
      l1: Logic1[In, SA, A],
      l2: Logic1[In, SB, B],
      l3: Logic1[In, SC, C])(
      mapOut: (A,B,C) => Z): StoreCache.Logic[In, Z] =
    new Logic3(l1, l2, l3, mapOut)

  final def apply4[In, SA,A, SB,B, SC,C, SD,D, Z](
      l1: Logic1[In, SA, A],
      l2: Logic1[In, SB, B],
      l3: Logic1[In, SC, C],
      l4: Logic1[In, SD, D])(
      mapOut: (A,B,C,D) => Z): StoreCache.Logic[In, Z] =
    new Logic4(l1, l2, l3, l4, mapOut)

  final def apply5[In, SA,A, SB,B, SC,C, SD,D, SE,E, Z](
      l1: Logic1[In, SA, A],
      l2: Logic1[In, SB, B],
      l3: Logic1[In, SC, C],
      l4: Logic1[In, SD, D],
      l5: Logic1[In, SE, E])(
      mapOut: (A,B,C,D,E) => Z): StoreCache.Logic[In, Z] =
    new Logic5(l1, l2, l3, l4, l5, mapOut)

  final def apply6[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, Z](
      l1: Logic1[In, SA, A],
      l2: Logic1[In, SB, B],
      l3: Logic1[In, SC, C],
      l4: Logic1[In, SD, D],
      l5: Logic1[In, SE, E],
      l6: Logic1[In, SF, F])(
      mapOut: (A,B,C,D,E,F) => Z): StoreCache.Logic[In, Z] =
    new Logic6(l1, l2, l3, l4, l5, l6, mapOut)

  final def apply7[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, Z](
      l1: Logic1[In, SA, A],
      l2: Logic1[In, SB, B],
      l3: Logic1[In, SC, C],
      l4: Logic1[In, SD, D],
      l5: Logic1[In, SE, E],
      l6: Logic1[In, SF, F],
      l7: Logic1[In, SG, G])(
      mapOut: (A,B,C,D,E,F,G) => Z): StoreCache.Logic[In, Z] =
    new Logic7(l1, l2, l3, l4, l5, l6, l7, mapOut)

  final def apply8[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, Z](
      l1: Logic1[In, SA, A],
      l2: Logic1[In, SB, B],
      l3: Logic1[In, SC, C],
      l4: Logic1[In, SD, D],
      l5: Logic1[In, SE, E],
      l6: Logic1[In, SF, F],
      l7: Logic1[In, SG, G],
      l8: Logic1[In, SH, H])(
      mapOut: (A,B,C,D,E,F,G,H) => Z): StoreCache.Logic[In, Z] =
    new Logic8(l1, l2, l3, l4, l5, l6, l7, l8, mapOut)

  final def apply9[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, Z](
      l1: Logic1[In, SA, A],
      l2: Logic1[In, SB, B],
      l3: Logic1[In, SC, C],
      l4: Logic1[In, SD, D],
      l5: Logic1[In, SE, E],
      l6: Logic1[In, SF, F],
      l7: Logic1[In, SG, G],
      l8: Logic1[In, SH, H],
      l9: Logic1[In, SI, I])(
      mapOut: (A,B,C,D,E,F,G,H,I) => Z): StoreCache.Logic[In, Z] =
    new Logic9(l1, l2, l3, l4, l5, l6, l7, l8, l9, mapOut)

  final def apply10[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, Z](
      l1: Logic1[In, SA, A],
      l2: Logic1[In, SB, B],
      l3: Logic1[In, SC, C],
      l4: Logic1[In, SD, D],
      l5: Logic1[In, SE, E],
      l6: Logic1[In, SF, F],
      l7: Logic1[In, SG, G],
      l8: Logic1[In, SH, H],
      l9: Logic1[In, SI, I],
      l10: Logic1[In, SJ, J])(
      mapOut: (A,B,C,D,E,F,G,H,I,J) => Z): StoreCache.Logic[In, Z] =
    new Logic10(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, mapOut)

  final def apply11[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, Z](
      l1: Logic1[In, SA, A],
      l2: Logic1[In, SB, B],
      l3: Logic1[In, SC, C],
      l4: Logic1[In, SD, D],
      l5: Logic1[In, SE, E],
      l6: Logic1[In, SF, F],
      l7: Logic1[In, SG, G],
      l8: Logic1[In, SH, H],
      l9: Logic1[In, SI, I],
      l10: Logic1[In, SJ, J],
      l11: Logic1[In, SK, K])(
      mapOut: (A,B,C,D,E,F,G,H,I,J,K) => Z): StoreCache.Logic[In, Z] =
    new Logic11(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, mapOut)

  final def apply12[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, Z](
      l1: Logic1[In, SA, A],
      l2: Logic1[In, SB, B],
      l3: Logic1[In, SC, C],
      l4: Logic1[In, SD, D],
      l5: Logic1[In, SE, E],
      l6: Logic1[In, SF, F],
      l7: Logic1[In, SG, G],
      l8: Logic1[In, SH, H],
      l9: Logic1[In, SI, I],
      l10: Logic1[In, SJ, J],
      l11: Logic1[In, SK, K],
      l12: Logic1[In, SL, L])(
      mapOut: (A,B,C,D,E,F,G,H,I,J,K,L) => Z): StoreCache.Logic[In, Z] =
    new Logic12(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, mapOut)

  final def apply13[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, Z](
      l1: Logic1[In, SA, A],
      l2: Logic1[In, SB, B],
      l3: Logic1[In, SC, C],
      l4: Logic1[In, SD, D],
      l5: Logic1[In, SE, E],
      l6: Logic1[In, SF, F],
      l7: Logic1[In, SG, G],
      l8: Logic1[In, SH, H],
      l9: Logic1[In, SI, I],
      l10: Logic1[In, SJ, J],
      l11: Logic1[In, SK, K],
      l12: Logic1[In, SL, L],
      l13: Logic1[In, SM, M])(
      mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M) => Z): StoreCache.Logic[In, Z] =
    new Logic13(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, mapOut)

  final def apply14[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, Z](
      l1: Logic1[In, SA, A],
      l2: Logic1[In, SB, B],
      l3: Logic1[In, SC, C],
      l4: Logic1[In, SD, D],
      l5: Logic1[In, SE, E],
      l6: Logic1[In, SF, F],
      l7: Logic1[In, SG, G],
      l8: Logic1[In, SH, H],
      l9: Logic1[In, SI, I],
      l10: Logic1[In, SJ, J],
      l11: Logic1[In, SK, K],
      l12: Logic1[In, SL, L],
      l13: Logic1[In, SM, M],
      l14: Logic1[In, SN, N])(
      mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N) => Z): StoreCache.Logic[In, Z] =
    new Logic14(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, mapOut)

  final def apply15[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, Z](
      l1: Logic1[In, SA, A],
      l2: Logic1[In, SB, B],
      l3: Logic1[In, SC, C],
      l4: Logic1[In, SD, D],
      l5: Logic1[In, SE, E],
      l6: Logic1[In, SF, F],
      l7: Logic1[In, SG, G],
      l8: Logic1[In, SH, H],
      l9: Logic1[In, SI, I],
      l10: Logic1[In, SJ, J],
      l11: Logic1[In, SK, K],
      l12: Logic1[In, SL, L],
      l13: Logic1[In, SM, M],
      l14: Logic1[In, SN, N],
      l15: Logic1[In, SO, O])(
      mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) => Z): StoreCache.Logic[In, Z] =
    new Logic15(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, mapOut)

  final def apply16[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, Z](
      l1: Logic1[In, SA, A],
      l2: Logic1[In, SB, B],
      l3: Logic1[In, SC, C],
      l4: Logic1[In, SD, D],
      l5: Logic1[In, SE, E],
      l6: Logic1[In, SF, F],
      l7: Logic1[In, SG, G],
      l8: Logic1[In, SH, H],
      l9: Logic1[In, SI, I],
      l10: Logic1[In, SJ, J],
      l11: Logic1[In, SK, K],
      l12: Logic1[In, SL, L],
      l13: Logic1[In, SM, M],
      l14: Logic1[In, SN, N],
      l15: Logic1[In, SO, O],
      l16: Logic1[In, SP, P])(
      mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) => Z): StoreCache.Logic[In, Z] =
    new Logic16(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16, mapOut)

  final def apply17[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, Z](
      l1: Logic1[In, SA, A],
      l2: Logic1[In, SB, B],
      l3: Logic1[In, SC, C],
      l4: Logic1[In, SD, D],
      l5: Logic1[In, SE, E],
      l6: Logic1[In, SF, F],
      l7: Logic1[In, SG, G],
      l8: Logic1[In, SH, H],
      l9: Logic1[In, SI, I],
      l10: Logic1[In, SJ, J],
      l11: Logic1[In, SK, K],
      l12: Logic1[In, SL, L],
      l13: Logic1[In, SM, M],
      l14: Logic1[In, SN, N],
      l15: Logic1[In, SO, O],
      l16: Logic1[In, SP, P],
      l17: Logic1[In, SQ, Q])(
      mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) => Z): StoreCache.Logic[In, Z] =
    new Logic17(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16, l17, mapOut)

  final def apply18[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, Z](
      l1: Logic1[In, SA, A],
      l2: Logic1[In, SB, B],
      l3: Logic1[In, SC, C],
      l4: Logic1[In, SD, D],
      l5: Logic1[In, SE, E],
      l6: Logic1[In, SF, F],
      l7: Logic1[In, SG, G],
      l8: Logic1[In, SH, H],
      l9: Logic1[In, SI, I],
      l10: Logic1[In, SJ, J],
      l11: Logic1[In, SK, K],
      l12: Logic1[In, SL, L],
      l13: Logic1[In, SM, M],
      l14: Logic1[In, SN, N],
      l15: Logic1[In, SO, O],
      l16: Logic1[In, SP, P],
      l17: Logic1[In, SQ, Q],
      l18: Logic1[In, SR, R])(
      mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) => Z): StoreCache.Logic[In, Z] =
    new Logic18(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16, l17, l18, mapOut)

  final def apply19[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, Z](
      l1: Logic1[In, SA, A],
      l2: Logic1[In, SB, B],
      l3: Logic1[In, SC, C],
      l4: Logic1[In, SD, D],
      l5: Logic1[In, SE, E],
      l6: Logic1[In, SF, F],
      l7: Logic1[In, SG, G],
      l8: Logic1[In, SH, H],
      l9: Logic1[In, SI, I],
      l10: Logic1[In, SJ, J],
      l11: Logic1[In, SK, K],
      l12: Logic1[In, SL, L],
      l13: Logic1[In, SM, M],
      l14: Logic1[In, SN, N],
      l15: Logic1[In, SO, O],
      l16: Logic1[In, SP, P],
      l17: Logic1[In, SQ, Q],
      l18: Logic1[In, SR, R],
      l19: Logic1[In, SS, S])(
      mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) => Z): StoreCache.Logic[In, Z] =
    new Logic19(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16, l17, l18, l19, mapOut)

  final def apply20[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, ST,T, Z](
      l1: Logic1[In, SA, A],
      l2: Logic1[In, SB, B],
      l3: Logic1[In, SC, C],
      l4: Logic1[In, SD, D],
      l5: Logic1[In, SE, E],
      l6: Logic1[In, SF, F],
      l7: Logic1[In, SG, G],
      l8: Logic1[In, SH, H],
      l9: Logic1[In, SI, I],
      l10: Logic1[In, SJ, J],
      l11: Logic1[In, SK, K],
      l12: Logic1[In, SL, L],
      l13: Logic1[In, SM, M],
      l14: Logic1[In, SN, N],
      l15: Logic1[In, SO, O],
      l16: Logic1[In, SP, P],
      l17: Logic1[In, SQ, Q],
      l18: Logic1[In, SR, R],
      l19: Logic1[In, SS, S],
      l20: Logic1[In, ST, T])(
      mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) => Z): StoreCache.Logic[In, Z] =
    new Logic20(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16, l17, l18, l19, l20, mapOut)

  final def apply21[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, ST,T, SU,U, Z](
      l1: Logic1[In, SA, A],
      l2: Logic1[In, SB, B],
      l3: Logic1[In, SC, C],
      l4: Logic1[In, SD, D],
      l5: Logic1[In, SE, E],
      l6: Logic1[In, SF, F],
      l7: Logic1[In, SG, G],
      l8: Logic1[In, SH, H],
      l9: Logic1[In, SI, I],
      l10: Logic1[In, SJ, J],
      l11: Logic1[In, SK, K],
      l12: Logic1[In, SL, L],
      l13: Logic1[In, SM, M],
      l14: Logic1[In, SN, N],
      l15: Logic1[In, SO, O],
      l16: Logic1[In, SP, P],
      l17: Logic1[In, SQ, Q],
      l18: Logic1[In, SR, R],
      l19: Logic1[In, SS, S],
      l20: Logic1[In, ST, T],
      l21: Logic1[In, SU, U])(
      mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) => Z): StoreCache.Logic[In, Z] =
    new Logic21(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16, l17, l18, l19, l20, l21, mapOut)

  final def apply22[In, SA,A, SB,B, SC,C, SD,D, SE,E, SF,F, SG,G, SH,H, SI,I, SJ,J, SK,K, SL,L, SM,M, SN,N, SO,O, SP,P, SQ,Q, SR,R, SS,S, ST,T, SU,U, SV,V, Z](
      l1: Logic1[In, SA, A],
      l2: Logic1[In, SB, B],
      l3: Logic1[In, SC, C],
      l4: Logic1[In, SD, D],
      l5: Logic1[In, SE, E],
      l6: Logic1[In, SF, F],
      l7: Logic1[In, SG, G],
      l8: Logic1[In, SH, H],
      l9: Logic1[In, SI, I],
      l10: Logic1[In, SJ, J],
      l11: Logic1[In, SK, K],
      l12: Logic1[In, SL, L],
      l13: Logic1[In, SM, M],
      l14: Logic1[In, SN, N],
      l15: Logic1[In, SO, O],
      l16: Logic1[In, SP, P],
      l17: Logic1[In, SQ, Q],
      l18: Logic1[In, SR, R],
      l19: Logic1[In, SS, S],
      l20: Logic1[In, ST, T],
      l21: Logic1[In, SU, U],
      l22: Logic1[In, SV, V])(
      mapOut: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) => Z): StoreCache.Logic[In, Z] =
    new Logic22(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16, l17, l18, l19, l20, l21, l22, mapOut)

  final def fn2[A: QuickEq, B: QuickEq, Z](f: (A,B) => Z): Logic1[(A,B), (A,B), Z] =
    StoreCache.Logic[(A,B), Z](x => f(x._1, x._2))

  final def fn3[A: QuickEq, B: QuickEq, C: QuickEq, Z](f: (A,B,C) => Z): Logic1[(A,B,C), (A,B,C), Z] =
    StoreCache.Logic[(A,B,C), Z](x => f(x._1, x._2, x._3))

  final def fn4[A: QuickEq, B: QuickEq, C: QuickEq, D: QuickEq, Z](f: (A,B,C,D) => Z): Logic1[(A,B,C,D), (A,B,C,D), Z] =
    StoreCache.Logic[(A,B,C,D), Z](x => f(x._1, x._2, x._3, x._4))

  final def fn5[A: QuickEq, B: QuickEq, C: QuickEq, D: QuickEq, E: QuickEq, Z](f: (A,B,C,D,E) => Z): Logic1[(A,B,C,D,E), (A,B,C,D,E), Z] =
    StoreCache.Logic[(A,B,C,D,E), Z](x => f(x._1, x._2, x._3, x._4, x._5))

  final def fn6[A: QuickEq, B: QuickEq, C: QuickEq, D: QuickEq, E: QuickEq, F: QuickEq, Z](f: (A,B,C,D,E,F) => Z): Logic1[(A,B,C,D,E,F), (A,B,C,D,E,F), Z] =
    StoreCache.Logic[(A,B,C,D,E,F), Z](x => f(x._1, x._2, x._3, x._4, x._5, x._6))

  final def fn7[A: QuickEq, B: QuickEq, C: QuickEq, D: QuickEq, E: QuickEq, F: QuickEq, G: QuickEq, Z](f: (A,B,C,D,E,F,G) => Z): Logic1[(A,B,C,D,E,F,G), (A,B,C,D,E,F,G), Z] =
    StoreCache.Logic[(A,B,C,D,E,F,G), Z](x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7))

  final def fn8[A: QuickEq, B: QuickEq, C: QuickEq, D: QuickEq, E: QuickEq, F: QuickEq, G: QuickEq, H: QuickEq, Z](f: (A,B,C,D,E,F,G,H) => Z): Logic1[(A,B,C,D,E,F,G,H), (A,B,C,D,E,F,G,H), Z] =
    StoreCache.Logic[(A,B,C,D,E,F,G,H), Z](x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8))

  final def fn9[A: QuickEq, B: QuickEq, C: QuickEq, D: QuickEq, E: QuickEq, F: QuickEq, G: QuickEq, H: QuickEq, I: QuickEq, Z](f: (A,B,C,D,E,F,G,H,I) => Z): Logic1[(A,B,C,D,E,F,G,H,I), (A,B,C,D,E,F,G,H,I), Z] =
    StoreCache.Logic[(A,B,C,D,E,F,G,H,I), Z](x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9))

  final def fn10[A: QuickEq, B: QuickEq, C: QuickEq, D: QuickEq, E: QuickEq, F: QuickEq, G: QuickEq, H: QuickEq, I: QuickEq, J: QuickEq, Z](f: (A,B,C,D,E,F,G,H,I,J) => Z): Logic1[(A,B,C,D,E,F,G,H,I,J), (A,B,C,D,E,F,G,H,I,J), Z] =
    StoreCache.Logic[(A,B,C,D,E,F,G,H,I,J), Z](x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10))

  final def fn11[A: QuickEq, B: QuickEq, C: QuickEq, D: QuickEq, E: QuickEq, F: QuickEq, G: QuickEq, H: QuickEq, I: QuickEq, J: QuickEq, K: QuickEq, Z](f: (A,B,C,D,E,F,G,H,I,J,K) => Z): Logic1[(A,B,C,D,E,F,G,H,I,J,K), (A,B,C,D,E,F,G,H,I,J,K), Z] =
    StoreCache.Logic[(A,B,C,D,E,F,G,H,I,J,K), Z](x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11))

  final def fn12[A: QuickEq, B: QuickEq, C: QuickEq, D: QuickEq, E: QuickEq, F: QuickEq, G: QuickEq, H: QuickEq, I: QuickEq, J: QuickEq, K: QuickEq, L: QuickEq, Z](f: (A,B,C,D,E,F,G,H,I,J,K,L) => Z): Logic1[(A,B,C,D,E,F,G,H,I,J,K,L), (A,B,C,D,E,F,G,H,I,J,K,L), Z] =
    StoreCache.Logic[(A,B,C,D,E,F,G,H,I,J,K,L), Z](x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12))

  final def fn13[A: QuickEq, B: QuickEq, C: QuickEq, D: QuickEq, E: QuickEq, F: QuickEq, G: QuickEq, H: QuickEq, I: QuickEq, J: QuickEq, K: QuickEq, L: QuickEq, M: QuickEq, Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M) => Z): Logic1[(A,B,C,D,E,F,G,H,I,J,K,L,M), (A,B,C,D,E,F,G,H,I,J,K,L,M), Z] =
    StoreCache.Logic[(A,B,C,D,E,F,G,H,I,J,K,L,M), Z](x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13))

  final def fn14[A: QuickEq, B: QuickEq, C: QuickEq, D: QuickEq, E: QuickEq, F: QuickEq, G: QuickEq, H: QuickEq, I: QuickEq, J: QuickEq, K: QuickEq, L: QuickEq, M: QuickEq, N: QuickEq, Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N) => Z): Logic1[(A,B,C,D,E,F,G,H,I,J,K,L,M,N), (A,B,C,D,E,F,G,H,I,J,K,L,M,N), Z] =
    StoreCache.Logic[(A,B,C,D,E,F,G,H,I,J,K,L,M,N), Z](x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14))

  final def fn15[A: QuickEq, B: QuickEq, C: QuickEq, D: QuickEq, E: QuickEq, F: QuickEq, G: QuickEq, H: QuickEq, I: QuickEq, J: QuickEq, K: QuickEq, L: QuickEq, M: QuickEq, N: QuickEq, O: QuickEq, Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) => Z): Logic1[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O), (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O), Z] =
    StoreCache.Logic[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O), Z](x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15))

  final def fn16[A: QuickEq, B: QuickEq, C: QuickEq, D: QuickEq, E: QuickEq, F: QuickEq, G: QuickEq, H: QuickEq, I: QuickEq, J: QuickEq, K: QuickEq, L: QuickEq, M: QuickEq, N: QuickEq, O: QuickEq, P: QuickEq, Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) => Z): Logic1[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), Z] =
    StoreCache.Logic[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), Z](x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15, x._16))

  final def fn17[A: QuickEq, B: QuickEq, C: QuickEq, D: QuickEq, E: QuickEq, F: QuickEq, G: QuickEq, H: QuickEq, I: QuickEq, J: QuickEq, K: QuickEq, L: QuickEq, M: QuickEq, N: QuickEq, O: QuickEq, P: QuickEq, Q: QuickEq, Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) => Z): Logic1[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q), (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q), Z] =
    StoreCache.Logic[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q), Z](x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15, x._16, x._17))

  final def fn18[A: QuickEq, B: QuickEq, C: QuickEq, D: QuickEq, E: QuickEq, F: QuickEq, G: QuickEq, H: QuickEq, I: QuickEq, J: QuickEq, K: QuickEq, L: QuickEq, M: QuickEq, N: QuickEq, O: QuickEq, P: QuickEq, Q: QuickEq, R: QuickEq, Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) => Z): Logic1[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R), (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R), Z] =
    StoreCache.Logic[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R), Z](x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15, x._16, x._17, x._18))

  final def fn19[A: QuickEq, B: QuickEq, C: QuickEq, D: QuickEq, E: QuickEq, F: QuickEq, G: QuickEq, H: QuickEq, I: QuickEq, J: QuickEq, K: QuickEq, L: QuickEq, M: QuickEq, N: QuickEq, O: QuickEq, P: QuickEq, Q: QuickEq, R: QuickEq, S: QuickEq, Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) => Z): Logic1[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S), (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S), Z] =
    StoreCache.Logic[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S), Z](x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15, x._16, x._17, x._18, x._19))

  final def fn20[A: QuickEq, B: QuickEq, C: QuickEq, D: QuickEq, E: QuickEq, F: QuickEq, G: QuickEq, H: QuickEq, I: QuickEq, J: QuickEq, K: QuickEq, L: QuickEq, M: QuickEq, N: QuickEq, O: QuickEq, P: QuickEq, Q: QuickEq, R: QuickEq, S: QuickEq, T: QuickEq, Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) => Z): Logic1[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T), (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T), Z] =
    StoreCache.Logic[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T), Z](x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15, x._16, x._17, x._18, x._19, x._20))

  final def fn21[A: QuickEq, B: QuickEq, C: QuickEq, D: QuickEq, E: QuickEq, F: QuickEq, G: QuickEq, H: QuickEq, I: QuickEq, J: QuickEq, K: QuickEq, L: QuickEq, M: QuickEq, N: QuickEq, O: QuickEq, P: QuickEq, Q: QuickEq, R: QuickEq, S: QuickEq, T: QuickEq, U: QuickEq, Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) => Z): Logic1[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U), (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U), Z] =
    StoreCache.Logic[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U), Z](x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15, x._16, x._17, x._18, x._19, x._20, x._21))

  final def fn22[A: QuickEq, B: QuickEq, C: QuickEq, D: QuickEq, E: QuickEq, F: QuickEq, G: QuickEq, H: QuickEq, I: QuickEq, J: QuickEq, K: QuickEq, L: QuickEq, M: QuickEq, N: QuickEq, O: QuickEq, P: QuickEq, Q: QuickEq, R: QuickEq, S: QuickEq, T: QuickEq, U: QuickEq, V: QuickEq, Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) => Z): Logic1[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V), (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V), Z] =
    StoreCache.Logic[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V), Z](x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15, x._16, x._17, x._18, x._19, x._20, x._21, x._22))
}
package com.beardedlogic.usecase.test

/**
 * @since 30/04/2013
 */
trait TestHelpers {

  def expectSoon(cond: => Any) {
    val test = (sleep: Int) => try { cond; true } catch { case _ => Thread.sleep(sleep); false }
    if (!test(50))
      if (!test(100))
        if (!test(100))
          if (!test(200))
            if (!test(500))
              if (!test(1000))
                if (!test(1000))
                  if (!test(1000))
                    if (!test(1000))
                      if (!test(1000))
                        cond
  }
}
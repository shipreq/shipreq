import tlc2.value.impl.IntValue;
import tlc2.value.impl.Value;

public final class Util {

  private static final UtilScala s = new UtilScala();

  public static Value NormaliseNats(final Value value, final IntValue maxGap, final IntValue setSize) {
    return s.normaliseNats(value, maxGap.val, setSize.val);
  }
}
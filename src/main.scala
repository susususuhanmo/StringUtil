import StringUtils.SafeString
import StringUtils.SafeString._
/**
  * Created by Administrator on 2018/4/3 0003.
  */

object main {


  def main(args: Array[String]): Unit = {
    var str = "000ab[ab]"
    str.safeDeleteLeftZero.Println
  }
}

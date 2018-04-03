package StringUtils

/**
  * Created by Administrator on 2018/4/3 0003.
  *
  * 一套可以隐式转换成String类型的字符串函数。
  * 1、重写自带的常用String函数，加入null值处理，在处理null值时不会抛出异常。
  * 2、加入一些常用字符串处理函数。如去除括号，判断是含有中文等等
  *
  * 使用方法：
  * 1、导入包含隐式转换的object：import StringUtils.SafeString.toMyString
  * 2、直接使用。例：
  *               "ab[ab]".safeDeleteBracket.safePrintln
  *
  *                 输出结果： ab
  *
  */
class SafeString(myStr: String) {

  implicit def toMyString(str: String): SafeString = new SafeString(str)


  /**
    * 功能：
    * 判断字符是否为Null值
    *
    * @return
    */
  def IsNull: Boolean = if (myStr==null) true else false

  def safeCharAt(n:Int) = if(myStr.safeLength-1 < n || n<0) null else myStr.charAt(n)

  /**
    * 功能：
    * 打印字符串
    */
  def Println: Unit = println(myStr)

  /**
    * 功能 ：
    * 整理应为null值的字符为null。（比如"","NULL"...等）
    *
    * @return String
    */
  def safeFormatNull: String =
    if (myStr.IsNull ||
      myStr == "NULL" ||
      myStr.safeTrim == "" ||
      myStr == "null")
      null
    else myStr

  /**
    * 功能：
    * 取字符串长度。
    *
    * @return
    */
  def safeLength: Int = if(myStr.IsNull) 0 else myStr.length


  /**
    * 功能：
    * 加入Null处理的trim函数
    *
    * @return String
    */
  def safeTrim: String = if (myStr.IsNull) null else myStr.trim

  /**
    * 功能：
    * 安全的indexOf
    *
    * @param str
    * @return
    */
  def safeIndexOf(str: String): Int = if (myStr.IsNull) -1 else myStr.indexOf(str)

  def safeIndexOf(char: Char): Int = myStr.safeIndexOf(char.toString)

  /**
    * 功能：
    * 安全的subString
    *
    * @param beginIndex
    * @return
    */
  def safeSubString(beginIndex: Int, endIndex: Int): String
  = if (myStr.IsNull) null else myStr.substring(beginIndex, endIndex)

  def safeSubString(beginIndex: Int): String = myStr.safeSubString(beginIndex,myStr.length)

  /**
    * 功能：
    * 取前几位的字符
    *
    * @param endIndex
    * @return
    */
  def safeHead(endIndex: Int): String = myStr.safeSubString(0, endIndex)
  /**
    * 功能：
    * 安全的left功能。
    *
    * @param beginIndex
    * @return
    */
  def safeTail(beginIndex: Int): String = myStr.safeSubString(beginIndex, myStr.safeLength -1)

  /**
    * 删除成对的中括号（以及内部内容）并删除不成对的中括号
    * @return
    */
  def safeDeleteBracket: String = {
    if(myStr.IsNull) return null
    val pattern = "\\[.*?\\]".r
    return pattern replaceAllIn(myStr, "")
  }

  /**
    * 删除成对的小括号（以及内部内容）并删除不成对的中括号
    * @return
    */
  def safeDeleteParentheses: String = {
    if(myStr.IsNull) return null
    val pattern = "\\(.*?\\)".r
    return pattern replaceAllIn(myStr, "") replace("(","") replace(")","")
  }

def safeDeleteLeftZero :String  = {
  var rtn = myStr.safeTrim
  while(rtn.safeCharAt(0) == '0'){
    if(rtn == "0") return "0"
    rtn = rtn.safeSubString(1)
  }
  rtn
}


//  def deleteZero(str : String) : String ={
//    if(str == null) null
//    else if (str.trim.length == 0) null
//    else {
//      var rtn =str.trim
//      while(rtn(0) == '0'){
//        if(rtn == "0") return "0"
//        rtn = rtn.substring(1)
//      }
//      rtn
//    }
//  }
}
object SafeString{
  implicit def toMyString(str: String): SafeString = new SafeString(str)

}



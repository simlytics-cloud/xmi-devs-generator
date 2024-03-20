package cloud.simlytics.devs.xmigenerator

object Generator {

  /**
   * Takes a camel cased identifier name and returns an underscore separated
   * name
   *
   * Example:
   *     camelToUnderscores("thisIsA1Test") == "this_is_a_1_test"
   */
  def camelToUnderscores(name: String) = {
    "[A-Z\\d]".r.replaceAllIn(name, { m =>
      "_" + m.group(0).toLowerCase()
    }).substring(1)
  }

  /* 
   * Takes an underscore separated identifier name and returns a camel cased one
   *
   * Example:
   *    underscoreToCamel("this_is_a_1_test") == "thisIsA1Test"
   */

  def underscoreToCamel(name: String) = "_([a-z\\d])".r.replaceAllIn(name, {m =>
    m.group(1).toUpperCase()
  })

  def lowerFirstLetter(s: String): String = {
    s.length match {
      case 0 => s
      case _ => s.substring(0, 1).toLowerCase + s.substring(1)
    }
  }

  def upperFirstLetter(s: String): String = {
    s.length match {
      case 0 => s
      case _ => s.substring(0, 1).toUpperCase() + s.substring(1)
    }
  }

  def splitTypeAndName(variable: String): (String, String) = {
    val typeAndName = variable.split("\\s+")
    if (typeAndName.length != 2) {
      throw new IllegalArgumentException(s"Bad value for port type and name: ${variable}.  Should be a type and a name " +
        s"separated by a space.")
    }
    typeAndName(0) -> typeAndName(1)
  }
}

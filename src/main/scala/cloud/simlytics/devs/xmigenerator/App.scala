package cloud.simlytics.devs.xmigenerator

import com.typesafe.config.ConfigFactory

/**
 * @author ${user.name}
 */
object App {



  def main(args: Array[String]) = {
    val config = args.length > 0 match {
      case true => ConfigFactory.load(args(0))
      case false => ConfigFactory.load()
    }

    val devsXmiFile = config.getString("devs.framework.xmi_file")
    val timeType = config.getString("devs.timeType")
    val systemXmiFile = config.getString("system.xmi_file")
    val appName = config.getString("system.app_name")
    val modelPackage = config.getString("system.model_package")
    val basePackage = config.getString("generator.base_package")
    val generatorSourceDir = config.getString("generator.src_folder_path")
    val xmlParser = XmiParser(scala.xml.XML.loadFile(devsXmiFile),
      scala.xml.XML.loadFile(systemXmiFile), modelPackage, basePackage, generatorSourceDir, timeType, appName)
  }

}

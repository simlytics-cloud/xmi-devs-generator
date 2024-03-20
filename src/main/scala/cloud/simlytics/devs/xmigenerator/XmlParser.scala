package cloud.simlytics.devs.xmigenerator

import scala.xml.Node

object XmlParser {

  def filterByAttributeValue(nodes: collection.Seq[Node], attribute: String, value: String): collection.Seq[Node] = {
    nodes.filter(node => {
      val attrMap: Map[String, String] = node.attributes.asAttrMap
      attrMap.getOrElse(attribute, null) == value
    })
  }

  def attributeValueOption(node: Node, attribute: String): Option[String] = {
    node.attributes.asAttrMap.get(attribute)
  }
  
  def attributeValue(node: Node, attribute: String): String = {
    attributeValueOption(node, attribute).getOrElse {
      throw new IllegalArgumentException(s"Node has no attribute \"${attribute}\":\n${node}")
    }
  }
  
}

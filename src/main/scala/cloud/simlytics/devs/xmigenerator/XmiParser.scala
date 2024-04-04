package cloud.simlytics.devs.xmigenerator

import cloud.simlytics.devs.xmigenerator.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.xml.*


class XmiParser(devsElement: Elem, modelFileElement: Elem, modelPackage: String, basePackage: String, generatorSourceDir: String,
                timeType: String, appName: String) {
  import XmlParser._
  import Generator._

  def buildDir(pkg: String): String = {
    val dir = generatorSourceDir + "/main/java/" + pkg.replace('.', '/') + "/"
    if (!Files.exists(Paths.get(dir))) {
      Files.createDirectory(Paths.get(dir))
    }
    dir
  }

  val modelElement = filterByAttributeValue((modelFileElement \\ "packagedElement"), "name", modelPackage);

  val immutablesPkg = basePackage + ".immutables"
  val devsClassNodes: collection.Seq[Node] = filterByAttributeValue((devsElement \\ "packagedElement") ++ (devsElement \\ "nestedClassifier"),
    "xmi:type", "uml:Class")
  val modelClassNodes: collection.Seq[Node] = filterByAttributeValue((modelElement \\ "packagedElement") ++ (modelElement \\ "nestedClassifier"),
    "xmi:type", "uml:Class") ++
    filterByAttributeValue((modelElement \\ "packagedElement") ++ (modelElement \\ "nestedClassifier"),
      "xmi:type", "uml:Enumeration")
  val modelAssociationNodes: collection.Seq[Node] = filterByAttributeValue((modelElement \\ "packagedElement") ++ (modelElement \\ "nestedClassifier"),
    "xmi:type", "uml:Association")

  val modelClassNameMap: Map[String, String] = buildClassNameMap(modelClassNodes)
  val modelClassIdMap: Map[String, String] = modelClassNameMap.map { case (k, v) =>
    v -> k
  }

  val modelNodeMap: Map[String, Node] = buildNodeMap(modelClassNodes)
  val devsNodeMap: Map[String, Node] = buildNodeMap(devsClassNodes)
  val modelAssociationMap: Map[String, Node] = buildNodeMap(modelAssociationNodes)

  val devsClassNameMap: Map[String, String] = buildClassNameMap(devsClassNodes)
  val devsClassIdMap: Map[String, String] = devsClassNameMap.map { case (k, v) =>
    v -> k
  }

  val mapProperties: List[String] = (modelFileElement \\ "Map").filter { node =>
    node.attribute("base_Property").nonEmpty
  }.map { node =>
    attributeValue(node, "base_Property")
  }.toList

  //val experimentalFrameNode = getNodesOfType(modelClassNodes, "ExperimentalFrame", devsClassNameMap).head
  val experimentalFrameNode = modelClassNodes.filter(n => getParentClassNames(n, devsNodeMap).contains("ExperimentalFrame")).head
  println("Experimental frame node is \n" + experimentalFrameNode)

  //val experimentalFrameAtomicModels = getNodesOfType((experimentalFrameNode \\ "nestedClassifier"), "DevsAtomicModel", devsClassNameMap)
  //val experimentalFrameCoupledModels = getNodesOfType((experimentalFrameNode \\ "nestedClassifier"), "DevsCoupledModel", devsClassNameMap)
  val experimentalFrameAtomicModels = (experimentalFrameNode \ "nestedClassifier")
    .filter(n => getParentClassNames(n, devsNodeMap).contains("DevsAtomicModel"))
  val experimentalFrameCoupledModels = (experimentalFrameNode \ "nestedClassifier")
    .filter(n => getParentClassNames(n, devsNodeMap).contains("DevsCoupledModel"))

  println("DEVS Experimental Frame Atomic Models: ")
  experimentalFrameAtomicModels.foreach(println(_))
  //experimentalFrameAtomicModels.foreach(generateDevsModel(_))

  println("DEVS Experimental Frame Coupled Models: ")
  experimentalFrameCoupledModels.foreach(println(_))
  //experimentalFrameCoupledModels.foreach(generateCoupledModel(_))
  generateCoupledModel(experimentalFrameNode, basePackage)


  println("Model class nodes:")
  modelClassNodes.map(node => attributeValueOption(node, "name").get).foreach(println)
  generatePlainImmutables()
  generateDevsStreamingApp(attributeValue(experimentalFrameNode, "name"))

  def generateDevsStreamingApp(experimentalFrameName: String): Unit = {
    val devsStreamingAppGenerator = new DevsStreamingAppGenerator(basePackage, appName, experimentalFrameName, timeType)
    val devsStreamingAppContent: String = devsStreamingAppGenerator.build()
    Files.write(Paths.get(buildDir(basePackage) + appName + ".java"),
      devsStreamingAppContent.getBytes(StandardCharsets.UTF_8))
  }

  def generateCoupledModel(coupledModelNode: Node, parentPackage: String): Unit = {
    val coupledModelName = attributeValue(coupledModelNode, "name")
    val coupledModelPackage = parentPackage + "." + coupledModelName.toLowerCase()
    val coupledModelDir = buildDir(coupledModelPackage)
    val subordinateAtomicModels = (coupledModelNode \ "nestedClassifier")
      .filter(n => getParentClassNames(n, devsNodeMap).contains("DevsAtomicModel"))
    val subordinateCoupledModels = (coupledModelNode \ "nestedClassifier")
      .filter(n => getParentClassNames(n, devsNodeMap).contains("DevsCoupledModel"))
    subordinateCoupledModels.foreach(generateCoupledModel(_, coupledModelPackage))

    val subordinateModels = subordinateCoupledModels ++ subordinateAtomicModels
    println("Subordinate Models: " + printNodeValues(subordinateModels, "name"))

    val subordinateModelsNames: List[String] = subordinateAtomicModels.map(attributeValue(_, "name")).toList
    val subordinateCoupledModelNames: List[String] = subordinateCoupledModels.map(attributeValue(_, "name")).toList

    // generate couplings
    val subordinatePorts: Map[String, Node] = subordinateModels.flatMap { subordinateModel =>
      accumulatePorts(getParentClasses(subordinateModel, modelNodeMap)).map { port =>
        attributeValue(port, "xmi:id") -> port
      }
    }.toMap
    println("Subordinate Ports: " + printNodeValues(subordinatePorts.values, "name"))
    val allPorts = subordinatePorts ++ accumulatePorts(getParentClasses(coupledModelNode, modelNodeMap)).map { port =>
      attributeValue(port, "xmi:id") -> port
    }.toMap
    println("All Ports: " + printNodeValues(allPorts.values, "name"))
    // Flow nodes for this coupling are those flows whose source and target are subordinate models or belong to this coupled model
    val flowNodes: Seq[Node] = (modelElement \\ "packagedElement").filter { flowNode =>
      attributeValueOption(flowNode, "xmi:type").contains("uml:InformationFlow") &&
        attributeValueOption(flowNode, "informationTarget").nonEmpty &&
        allPorts.keys.toSeq.contains(attributeValue(flowNode, "informationTarget"))  &&
        attributeValueOption(flowNode, "informationSource").nonEmpty &&
        allPorts.keys.toSeq.contains(attributeValue(flowNode, "informationSource"))
    }
    println("Flow nodes: " + printNodeValues(flowNodes, "name"))
    val flows: Seq[ItemFlow] = flowNodes.map { flowNode =>
      val fromPortNode: Node = try {
        allPorts(attributeValue(flowNode, "informationSource"))
      } catch {
        case e: Exception =>
          println(s"Flow node below's information source ${attributeValue(flowNode, "informationSource")}"
          + " is not in the allPorts map\n" + flowNode)
          null
      }
      val toPortNode = try {
        allPorts(attributeValue(flowNode, "informationTarget"))
      } catch {
        case e: Exception =>
          println(s"Flow node below's information target ${attributeValue(flowNode, "informationTarget")}"
            + " is not in the allPorts map\n" + flowNode)
          null
      }

      ItemFlow(
        fromModel = {
          //modelClassIdMap(attributeValue(getParentClass(fromPortNode), "xmi:id"))
          val fromModelNode: Node = (subordinateModels ++ coupledModelNode).find { model =>
            accumulatePorts(getParentClasses(model, modelNodeMap)).map(attributeValue(_, "xmi:id"))
              .contains(attributeValue(fromPortNode, "xmi:id"))
          }.getOrElse {
            throw new IllegalArgumentException(s"Could not find model for port ${attributeValue(fromPortNode, "name")}")
          }
          attributeValue(fromModelNode, "name")
        },
        fromPort = lowerFirstLetter(attributeValue(fromPortNode, "name")),
        toModel = {
          //modelClassIdMap(attributeValue(getParentClass(toPortNode), "xmi:id"))
          val toModelNode = (subordinateModels ++ coupledModelNode).find { model =>
            accumulatePorts(getParentClasses(model, modelNodeMap)).map(attributeValue(_, "xmi:id"))
              .contains(attributeValue(toPortNode, "xmi:id"))
          }.getOrElse {
            throw new IllegalArgumentException(s"Could not find model for port ${attributeValue(fromPortNode, "name")}")
          }
          attributeValue(toModelNode, "name")
        },
        toPort = lowerFirstLetter(attributeValue(toPortNode, "name")),
        toPortType = modelClassIdMap(attributeValue(flowNode, "conveyed"))
      )
    }

    subordinateAtomicModels.foreach(generateDevsModel(_, coupledModelPackage, flows))
    val couplingsGenerator = new CouplingsGenerator(coupledModelPackage, basePackage, immutablesPkg, coupledModelName, flows.toList, subordinateCoupledModelNames)
    println(couplingsGenerator.buildOutputCouplings())
    println(couplingsGenerator.buildInputCouplings())
    val inputCouplingsContent = couplingsGenerator.buildInputCouplings()
    Files.write(Paths.get(coupledModelDir + coupledModelName + "InputCouplingHandler.java"),
      inputCouplingsContent.getBytes(StandardCharsets.UTF_8))
    val outputCouplingsContent = couplingsGenerator.buildOutputCouplings()
    Files.write(Paths.get(coupledModelDir + coupledModelName + "OutputCouplingHandler.java"),
      outputCouplingsContent.getBytes(StandardCharsets.UTF_8))

    val modelParents = getParentClasses(coupledModelNode, modelNodeMap)
    val modelPorts = accumulatePorts(modelParents)
    val modelPortValues = modelPorts.map(toClassNameType(_)).toList
    val coupledModelGenerator = new CoupledModelGenerator(coupledModelPackage, immutablesPkg, coupledModelName, timeType, modelPortValues,
      subordinateModelsNames, subordinateCoupledModelNames)
    val coupledModelContent: String = coupledModelGenerator.buildModel()
    Files.write(Paths.get(coupledModelDir + coupledModelName + ".java"),
      coupledModelContent.getBytes(StandardCharsets.UTF_8))
    val coupledModelFactoryContent: String = coupledModelGenerator.buildFactory()
    Files.write(Paths.get(coupledModelDir + "Abstract" + coupledModelName + "Factory.java"),
      coupledModelFactoryContent.getBytes(StandardCharsets.UTF_8))
  }

  def printNodeValues(nodes: Iterable[Node], value: String): String = {
    nodes.map(printNodeValues(_, value)).mkString(", ")
  }

  def printNodeValues(node: Node, value: String): String = {
    attributeValueOption(node, value).getOrElse("Not found")
  }

  def generateDevsModel(modelNode: Node, modelPackage: String, allFlows: Seq[ItemFlow]): Unit = {
    val modelName = attributeValue(modelNode, "name")
    val modelDirectory = buildDir(modelPackage)
    val modelParents = getParentClasses(modelNode, modelNodeMap)

    println(s"${modelName} Model Hierarchy: \n")
    modelParents.foreach(println(_))

    println(s"${modelName} Variables:\n")
    val modelVariables = accumulateState(modelParents)
    modelVariables.foreach(println)

    println(s"${modelName} ports:")
    val modelPorts = accumulatePorts(modelParents)
    modelPorts.foreach(println)

    println (s"${modelName} port variables:\n")
    modelPorts.foreach(node => println(toClassNameType(node)))

    val modelState = modelVariables.filter(attributeValueOption(_, "isReadOnly") != Some("true"))
    val modelProperties = modelVariables.filter(attributeValueOption(_, "isReadOnly") == Some("true"))

    val modelOutputFlows = allFlows.filter { flow =>
      flow.fromModel == modelName
    }

    val pendingOutputPortVariables = modelOutputFlows.map(f =>
      Parameter(s"List<${f.toPortType}>", s"pending${upperFirstLetter(f.fromPort)}Out")).toList

    val modelStateParameters = modelState.map(toClassNameType(_)).toList ++ pendingOutputPortVariables

    println(s"${modelName} Java internal state variables:")
    modelStateParameters.foreach(node => println(_))
    val stateGenerator = new ImmutableGenerator(modelName + "State", modelPackage, immutablesPkg,
      modelStateParameters, true)
    generateImmutable(stateGenerator, modelDirectory)

    println(s"${modelName} Java Properties:")
    modelProperties.foreach(node => println(toClassNameType(node)))
    val propertiesGenerator = new ImmutableGenerator(modelName + "Properties", modelPackage, immutablesPkg,
      modelProperties.map(toClassNameType(_)).toList)
    generateImmutable(propertiesGenerator, modelDirectory)

    val operations: List[OwnedOperation] = accumulateOperations(modelParents).map { node =>
      val operationName = attributeValue(node, "name")
      val parameters: List[Parameter] = (node \ "ownedParameter")
        .filter(parameterNode => attributeValueOption(parameterNode, "direction") != Some("out"))
        .map { parameterNode =>
        toClassNameType(parameterNode)
      }.toList

      val result: Parameter = (node \ "ownedParameter")
        .filter(node => attributeValueOption(node, "direction") == Some("out"))
        .map { parameterNode =>
          toClassNameType(parameterNode)
        }.headOption.getOrElse(Parameter("void", "void"))

      val commentOption = (node \ "ownedComment" \ "body").headOption.map(_.text)

      OwnedOperation(operationName, parameters, result, commentOption)
    }.toList

    val outputPorts = modelPorts.filter(port => modelOutputFlows.map(f => f.fromPort).contains(lowerFirstLetter(attributeValue(port, "name"))))
    val inputPorts = modelPorts.filter(port => !modelOutputFlows.map(f => f.fromPort).contains(lowerFirstLetter(attributeValue(port, "name"))))

    val modelGenerator = ModelGenerator(modelName, modelPackage, immutablesPkg, timeType, modelProperties.map(toClassNameType(_)).toList,
      modelState.map(toClassNameType(_)).toList, inputPorts.map(toClassNameType(_)).toList,
      outputPorts.map(toClassNameType(_)).toList, operations)
    val fileContents = modelGenerator.buildModel()
    println(fileContents)
    Files.write(Paths.get(modelDirectory + modelName + ".java"), fileContents.getBytes(StandardCharsets.UTF_8))

  }

  def generateImmutable(immutableGenerator: ImmutableGenerator, directory: String): Unit = {
    val fileContents = immutableGenerator.build();
    println(fileContents)
    Files.write(Paths.get(directory + "Abstract" + immutableGenerator.className + ".java"),
      fileContents.getBytes(StandardCharsets.UTF_8))
  }

  def generatePlainImmutables(): Unit = {
    val immutablePackage = basePackage + ".immutables"
    val immutablesDir = buildDir(immutablePackage)
    modelClassNodes.filter {modelNode =>
      val devsParents = getParentClassNames(modelNode, devsNodeMap).tail
      val nodeName = attributeValueOption(modelNode, "name").get
      println(s"${nodeName} has DEVS parents " + devsParents)
      devsParents.isEmpty
    }.foreach { modelNode =>
      val className = attributeValueOption(modelNode, "name").getOrElse {
        throw new IllegalArgumentException("Model node has no \"name\" attribute.\n" + modelNode)
      }
      val parents = getParentClasses(modelNode, modelNodeMap)
      val state = accumulateState(parents)
      val variables = state.map(toClassNameType(_)).toList
      val generator = ImmutableGenerator(className = className, pkg = immutablePackage, immutablesPkg, variables = variables)
      generateImmutable(generator, immutablesDir)
    }
  }

  def buildClassNameMap(nodes: collection.Seq[Node]): Map[String, String] = {
    nodes.map { node =>
      attributeValueOption(node, "name").getOrElse("") -> attributeValueOption(node, "xmi:id").getOrElse("")
    }.toMap.filter { case (k,v) => k.nonEmpty && v.nonEmpty}
  }

  def buildNodeMap(nodes: collection.Seq[Node]): Map[String, Node] = {
    nodes.map { node =>
      attributeValueOption(node, "xmi:id").getOrElse("") -> node
    }.toMap.filter { case (k,v) => k.nonEmpty}
  }


  def getNodesOfType(nodes: collection.Seq[Node], nodeType: String, classNameMap: Map[String, String]): collection.Seq[Node] = {
    nodes.filter(classNode => {
      filterByAttributeValue((classNode \ "generalization" \ "general"), "href",
        "DEVSFramework.uml#" + devsClassNameMap(nodeType)).nonEmpty
    })
  }

  def getParentClassNames(baseNode: Node, nodeMap: Map[String, Node]): collection.Seq[String] = {
    getParentClasses(baseNode, nodeMap).map { node =>
      attributeValueOption(node, "name").getOrElse {
        throw new IllegalArgumentException("Node has no \"name\" attribute.\n" + node)
      }
    }
  }

  def getParentClass(currentNode: Node): Node = {
    modelNodeMap.values.filter(n => n.child.contains(currentNode)).head
  }

  def getParentClasses(baseNode: Node, nodeMap: Map[String, Node]): collection.Seq[Node] = {
    @tailrec
    def parentAccumulator(parents: collection.Seq[Node], currentNode: Node): collection.Seq[Node] = {
      val generalizations: collection.Seq[Node] = (currentNode \ "generalization")
        .filter(n => {
          n.attributes.asAttrMap.contains("general") || (n \ "general").nonEmpty
      }).map { n =>
        n.attributes.asAttrMap.contains("general") match {
          case true => nodeMap.get (attributeValueOption (n, "general").get)
          case false =>
            val id = attributeValue((n \ "general").head, "href").split("#").last
            nodeMap.get(id)
        }
      }.flatten
      generalizations.headOption match {
        case Some(parentNode) => parentAccumulator(parents :+ parentNode, parentNode)
        case None => parents
      }
    }
    parentAccumulator(collection.Seq(baseNode), baseNode)
  }

  def accumulateState(classHierarchy: collection.Seq[Node]): collection.Seq[Node] = {
    classHierarchy.flatMap { node =>
      filterByAttributeValue((node \ "ownedAttribute"), "xmi:type", "uml:Property")
    }
  }

  def accumulatePorts(classHierarchy: collection.Seq[Node]): collection.Seq[Node] = {
    classHierarchy.flatMap { node =>
      filterByAttributeValue((node \ "ownedAttribute"), "xmi:type", "uml:Port")
    }
  }

  def accumulateOperations(classHierarchy: collection.Seq[Node]): collection.Seq[Node] = {
    classHierarchy.flatMap { node =>
      filterByAttributeValue((node \ "ownedOperation"), "xmi:type", "uml:Operation")
    }
  }

  def toClassNameType(propertyNode: Node): Parameter = {
    val propertyName: String = attributeValueOption(propertyNode, "name").getOrElse {
      throw new IllegalArgumentException("The state node below has no \"name\" attribute.\n" + propertyNode)
    }
    // The property is a class with a specific id
    val propertyClass: String = attributeValueOption(propertyNode, "type") match {
      case Some(id) =>
        val className = modelClassIdMap.getOrElse(id,{
        throw new IllegalArgumentException(s"No matching class for type id ${id} in the property node below:\n"
          + propertyNode)
        })
        determineMultiplicity(propertyNode, className)
      case None =>
        (propertyNode \ "type").headOption match {
          case Some(typeNode) =>
            // The property is a primitive type
            val typeAttribute = attributeValueOption(typeNode, "href").getOrElse {
              throw new IllegalArgumentException("The type node below has no \"href\" attribute\n" + typeNode)
            }
            val className = typeAttribute match {
              case x if x.startsWith("DEVSFramework.uml#") =>
                devsClassIdMap(typeAttribute.substring("DEVSFramework.uml#".length))
              case x if x.contains("#String") => "String"
              case x if x.contains("#Real") => "Double"
              case x if x.contains("#Integer") => "Integer"
              case x if x.contains("Boolean") => "Boolean"
              case x if x.contains("#Complex") => "Complex"
              case x =>
                throw new IllegalArgumentException(s"Did not recognize primitive type ${x} in node below\n" + typeNode)
            }
            determineMultiplicity(propertyNode, className)
          case None =>
            val associationId: String = attributeValueOption(propertyNode, "association") match {
              case Some(a) => a
              case None =>
                throw new IllegalArgumentException("Could not identify the property type for the node below.  It has" +
                  " no type attribute, association attribute, or type element.\n" + propertyNode)
            }
            val associationNode: Node = modelAssociationMap.getOrElse(associationId, {
              throw new IllegalArgumentException(s"The association id ${associationId} for the property node below " +
              s"has now corresponding association in the modelAssociationMap\n" + propertyNode)
            })
            // In the XMI file, an association node has a memberEnd attribute with each member separated by a space
            // It has a ownedEnd element with an association attribute that identifies the owning member
            // The property's type has the class name of the non-owned member end
            val memberEnds: List[String] = attributeValueOption(associationNode, "memberEnd").getOrElse({
              throw new IllegalArgumentException("The association node below has no memberEnd attribute\n" +
                associationNode)
            }).split("\\s+").toList
            val ownedEnd: String = (associationNode \ "ownedEnd").headOption match {
              case Some(ownedEndElement) =>
                attributeValueOption(ownedEndElement, "association").getOrElse {
                  throw new IllegalArgumentException("The ownedEnd element of the association node below has no " +
                    "association attribute\n" + associationNode)
                }
              case None =>
                throw new IllegalArgumentException("The association node below has no ownedEnd element\n" +
                  associationNode)
            }
            val propertyClassId = memberEnds.filterNot(_.equals(ownedEnd)).headOption.getOrElse({
              throw new IllegalArgumentException(s"Could not determine class type of non-member end for association " +
                s"below with member ends ${memberEnds}\n" + associationNode)
            })
            val className: String = modelClassIdMap.getOrElse(propertyClassId, {
              throw new IllegalArgumentException(s"When processing association node below, could not find classId " +
                s"${propertyClassId} in modelClassNameMap\n" + associationNode)
            })
            // Determine the property collection type based on the following
            determineMultiplicity(associationNode, className)
        }
    }
    val comment: Option[String] = (propertyNode \ "ownedComment" \ "body").headOption.map(_.text)
    Parameter(propertyClass, propertyName, comment)
  }

  /**
   * Returns the Java class type of a node given its cardinality.  It looks for upperValue and lowerValue elements
   * to determine if the property represented by the node is a single value, collection, or optionally null value
   * @param node an XMI ownedAttribute or packagedElement representing an association for an owned attribute
   * @param className the Java class name for the property of unknown cardinality
   * @return the type of the property based on its cardinality.  For a single value, it will be the className.
   *         For a multiple valued collection, it will be a List of the class.
   *         For an optional value, it will be a Java Optional wrapper of the class
   */
  def determineMultiplicity(node: Node, className: String): String = {
    (node \ "upperValue").headOption match {
      case Some(upperNode) =>
        val upperValue: String = attributeValueOption(upperNode, "value").getOrElse {
          throw new IllegalArgumentException("upperValue element in node below has not \"value\" attribute.\n" + node)
        }
        upperValue match {
          case "*" =>
            mapProperties.contains(attributeValueOption(node, "xmi:id").getOrElse("")) match {
              case true =>
                // This property is a Map value.  It should contain two attributes, the key first, then the value
                val classNode: Node = modelClassNodes.find(attributeValue(_, "name") == className).getOrElse {
                  throw new IllegalArgumentException(s"Could not fine class name ${className} in modelClassNodes" )
                }
                val (key, value) = {
                  val parents = getParentClasses(classNode, modelNodeMap)
                  val state = accumulateState(parents)
                  val variables = state.map(toClassNameType).toList
                  if (variables.length != 2) {
                    throw new IllegalArgumentException(
                      s"A Map variable should have two properties.  ${className} has the properties " +
                        s"${variables.map(_.name).mkString(", ")}")
                  }
                  (variables(0).parameterType, variables(1).parameterType)
                }
                s"Map<${key}, ${value}>"
              case _ => s"List<${className}>"
            }

          case x if x.contains("..") => s"List<${className}>"
          case _ =>
            try {
              if (upperValue.toInt > 1) {
                s"List<${className}>"
              } else {
                className
              }
            } catch {
              case e: Exception =>
                throw new IllegalArgumentException("Unrecognized cardinality ")
            }
        }
      case None => className
    }
  }





}

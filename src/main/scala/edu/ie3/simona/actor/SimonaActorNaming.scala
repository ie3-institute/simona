/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.actor

import akka.actor.{ActorRef, ActorRefFactory, Props}

import java.util.UUID

/** In general, actor names in SIMONA consist of type name and actor id:
  *
  * `actorName = typeName + "_" + actorId`
  *
  * Or, in case of singletons, the actor name can consist of the type name only.
  *
  * Actor ref paths are structured depending on the type of reference.
  *
  * '''Local actors'''
  *
  * `akka://simona/user/-parent-/-name-`
  *
  * For example:
  * `akka://simona/user/SimonaSim/GridAgent_136/FixedLoadAgent_33cd1941-51f2-425a-8f4d-67e00d0b1876`
  *
  * '''Singletons'''
  *
  * The self-reference: `akka://simona/user/-name-/singleton`
  *
  * The actor when created: `akka://simona/user/-name-`
  *
  * A proxy ref: `akka://simona/user/$a` (does not follow pattern)
  *
  * '''Sharded actors'''
  *
  * `akka://simona/system/sharding/-typename-/-shardHash-/-name-`
  *
  * For example:
  * `akka://simona/system/sharding/FixedLoadAgent/5/FixedLoadAgent_0211454c-9be4-470f-9cf5-cef9b87cb640`
  *
  * This has been verified with Akka version 2.6.17.
  */
object SimonaActorNaming {

  @Deprecated
  implicit class RichActorRefFactory(private val refFactory: ActorRefFactory)
      extends AnyVal {

    def simonaActorOf(props: Props, actorId: String): ActorRef =
      refFactory.actorOf(props, actorName(props, actorId))

    def simonaActorOf(props: Props): ActorRef =
      refFactory.actorOf(props, actorName(props, simonaActorUuid))
  }

  /** Constructs a uuid and cuts it down to 6 digits for convenience. Although
    * this is dangerous as duplicates might be possible, it should be sufficient
    * in our case as the uniqueness is only required in one actor system
    *
    * @return
    *   a shortened uuid string
    */
  private def simonaActorUuid: String =
    UUID.randomUUID().toString.substring(0, 5)

  /** Constructs an actor name based on the simona convention for actor names.
    * The provided combination of class and id has to be unique for the whole
    * actor system
    *
    * @return
    *   the actor name based on simona conventions as string
    */
  def actorName(props: Props, actorId: String): String =
    actorName(typeName(props), actorId)

  /** Constructs an actor name based on the simona convention for actor names.
    * The provided combination of class and id has to be unique for the whole
    * actor system
    *
    * @return
    *   the actor name based on simona conventions as string
    */
  def actorName(clz: Class[_], actorId: String): String =
    actorName(typeName(clz), actorId)

  /** Constructs an actor name based on the simona convention for actor names.
    * The provided combination of type name and actor id has to be unique for
    * the whole actor system
    *
    * @return
    *   the actor name based on simona conventions as string
    */
  def actorName(typeName: String, actorId: String): String =
    s"${typeName}_${cleanActorIdForAkka(actorId)}"

  /** Extracts the actor name from given [[ActorRef]]. Cluster singletons are
    * taken care of separately.
    *
    * @return
    *   the actor name extract from the ActorRef
    */
  def actorName(actorRef: ActorRef): String =
    actorRef.path.name match {
      case "singleton" =>
        // singletons end in /${actorName}/singleton
        actorRef.path.parent.name
      case other =>
        other
    }

  /** Constructs the type name from given props.
    *
    * @return
    *   the type name
    */
  def typeName(props: Props): String = {
    props.args.headOption
      .flatMap {
        case clz: Class[_] => Some(clz)
        case _             => None
      }
      .map(clz => typeName(clz))
      .getOrElse(
        throw new RuntimeException(
          s"Cannot derive actor class from props: $props"
        )
      )
  }

  /** Constructs the type name from given class.
    *
    * @return
    *   the type name
    */
  def typeName(clz: Class[_]): String =
    clz.getSimpleName.replace("$", "")

  def typeName(actorRef: ActorRef): String = {
    val name = actorName(actorRef)
    if (name.contains('_'))
      name.split("_", 2)(0) // most actorNames match TypeName_ActorId
    else
      name // Some Singleton actorNames match TypeName
  }

  /** Akka prevents the usage of specific special characters as names. This
    * method cleans a given string and makes it usable as actor name
    *
    * @param inputString
    *   the uncleaned input string
    * @return
    *   a cleaned string that can be used as actor name
    */
  private def cleanActorIdForAkka(inputString: String): String = {
    val regexFilter =
      "[^A-Za-z0-9-_.*$+:@&=,!~';.]" // akka prevents using other special chars than the ones here
    inputString.replaceAll(regexFilter, "")
  }

}

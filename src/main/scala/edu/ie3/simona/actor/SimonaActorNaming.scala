/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.actor

import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps
import org.apache.pekko.actor.{ActorRefFactory, Props, ActorRef => ClassicRef}

import scala.util.Random

object SimonaActorNaming {

  implicit class RichActorRefFactory(private val refFactory: ActorRefFactory)
      extends AnyVal {

    def simonaActorOf(props: Props, actorId: String): ClassicRef =
      refFactory.actorOf(props, actorName(props, simonaActorId(actorId)))
  }

  /** Constructs an Id for convenience actor naming. Although this is dangerous
    * as duplicates might be possible, it should be sufficient in our case as
    * the uniqueness is only required in one actor system
    *
    * @return
    *   an Id string
    */
  private def simonaActorId(actorId: String): String = {
    val randomNumber = Random.nextInt(1000).toString
    s"$actorId-$randomNumber"
  }

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
    s"${typeName}_${cleanActorIdForPekko(actorId)}"

  /** Extracts the actor name from given [[ClassicRef]]. Cluster singletons are
    * taken care of separately.
    *
    * @return
    *   the actor name extract from the ActorRef
    */
  def actorName(actorRef: ClassicRef): String =
    actorRef.path.name match {
      case "singleton" =>
        // singletons end in /${actorName}/singleton
        actorRef.path.parent.name
      case other =>
        other
    }

  /** Extracts the actor name from given [[ActorRef]]. Cluster singletons are
    * taken care of separately.
    *
    * @return
    *   the actor name extract from the ActorRef
    */
  def actorName(actorRef: ActorRef[_]): String = actorName(actorRef.toClassic)

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

  /** Pekko prevents the usage of specific special characters as names. This
    * method cleans a given string and makes it usable as actor name
    *
    * @param inputString
    *   the uncleaned input string
    * @return
    *   a cleaned string that can be used as actor name
    */
  private def cleanActorIdForPekko(inputString: String): String = {
    val regexFilter =
      "[^A-Za-z0-9-_.*$+:@&=,!~';.]" // pekko prevents using other special chars than the ones here
    inputString.replaceAll(regexFilter, "")
  }

}

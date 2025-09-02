/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.actor

import org.apache.pekko.actor.typed.ActorRef

object SimonaActorNaming {

  /** Constructs an actor name based on the simona convention for actor names.
    * The provided combination of class and id has to be unique for the whole
    * actor system
    *
    * @return
    *   the actor name based on simona conventions as string
    */
  def actorName(clz: Class[?], actorId: String): String =
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

  /** Extracts the actor name from given [[ActorRef]]. Cluster singletons are
    * taken care of separately.
    *
    * @return
    *   the actor name extract from the ActorRef
    */
  def actorName(actorRef: ActorRef[?]): String = actorRef.path.name match {
    case "singleton" =>
      // singletons end in /${actorName}/singleton
      actorRef.path.parent.name
    case other =>
      other
  }

  /** Constructs the type name from given class.
    *
    * @return
    *   the type name
    */
  def typeName(clz: Class[?]): String =
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

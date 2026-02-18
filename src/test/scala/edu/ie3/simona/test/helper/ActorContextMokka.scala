/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.helper

import org.apache.pekko.actor.testkit.typed.scaladsl.ActorTestKitBase
import org.apache.pekko.actor.typed.{ActorRef, Behavior, Props}
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.mockito.ArgumentMatchers.{any, anyString}
import org.mockito.Mockito.when
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.Answer
import org.scalatestplus.mockito.MockitoSugar.mock

/** Trait that provides a mock for an [[ActorContext]].
  */
trait ActorContextMokka {
  this: ActorTestKitBase =>

  /** Create the actor context mock.
    * @tparam T
    *   Type of the context.
    * @return
    *   The mocked actor context.
    */
  def getMock[T]: ActorContext[T] = {
    val context = mock[ActorContext[T]]

    object Call extends Answer[ActorRef[T]] {
      override def answer(invocation: InvocationOnMock): ActorRef[T] = {
        val arguments: Array[Object] = invocation.getArguments
        val behavior = arguments(0).asInstanceOf[Behavior[T]]
        val name = arguments(1).asInstanceOf[String]

        ActorContextMokka.this.spawn(behavior, name)
      }
    }

    when(context.spawn(any[Behavior[T]](), anyString(), any[Props]()))
      .thenAnswer(Call)

    context
  }
}

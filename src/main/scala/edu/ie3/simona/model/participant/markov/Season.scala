import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

object Season extends Enumeration {
  type Season = Value
  val SPRING, SUMMER, AUTUMN, WINTER = Value

  def getSeason(dateTime: LocalDateTime): Season = {
    val month = dateTime.getMonthValue
    if (month >= 3 && month <= 5) SPRING
    else if (month >= 6 && month <= 8) SUMMER
    else if (month >= 9 && month <= 11) AUTUMN
    else WINTER
  }
}

object DayType extends Enumeration {
  type DayType = Value
  val WEEKDAY, SATURDAY, SUNDAY = Value

  def getDayType(dateTime: LocalDateTime): DayType = {
    val weekday = dateTime.getDayOfWeek.getValue
    if (weekday < 6) WEEKDAY
    else if (weekday == 6) SATURDAY
    else SUNDAY
  }
}

case class TimeInterval(start: LocalDateTime, end: LocalDateTime) {
  def isWithin(time: LocalDateTime): Boolean = {
    start.compareTo(time) <= 0 && end.compareTo(time) > 0
  }
}

object TimeInterval {
  def getOperationInterval(start: LocalDateTime, duration: Long): TimeInterval = {
    val end = start.plus(duration, ChronoUnit.MILLIS)
    TimeInterval(start, end)
  }
}

object Main extends App {
  val now = LocalDateTime.now()

  println("Current Season: " + Season.getSeason(now))
  println("Current Day Type: " + DayType.getDayType(now))

  val operationInterval = TimeInterval.getOperationInterval(now, 3600000) // 1 hour duration
  println("Is within operation interval? " + operationInterval.isWithin(now))
}


import cats.Eval
import cats.data.{State, _}

object StateMonadExample extends App {
  // Case class that holds the settings
  case class Settings(count: Int, colour: String, participants: Seq[String])

  // Class/method that sets the count
  def setCount(newCounter: Int): State[Settings, Int] = State[Settings, Int] { settings =>
    val oldCounter = settings.count
    val newSettings = settings.copy(
      count = newCounter
    )

    (newSettings, oldCounter)
  }

  // Class/method that updates the colour
  def updateColour(colour: String): State[Settings, String] = State[Settings, String] { settings =>
    val sanitizedColour = colour.toUpperCase()
    val newSettings = settings.copy(
      colour = sanitizedColour,
      count = settings.count + 1
    )

    (newSettings, sanitizedColour)
  }

  // Class/method that adds a new participant
  def addParticipant(person: String): State[Settings, Seq[String]] = State[Settings, Seq[String]] { settings =>
    val newSettings = settings.copy(
      participants = settings.participants :+ s"Name: $person",
      count = settings.count + 1
    )

    (newSettings, newSettings.participants)
  }

  // Creating (but not running yet) a pipeline that contains the steps that consume the status and
  // do some elaboration, returning a value and updating the settings
  val finalSettingsState: IndexedStateT[Eval, Settings, Settings, Seq[String]] = for {
    _         <- setCount(4)
    theColour <- updateColour("red")
    _         <- addParticipant("Fabrizio")
    thePeople <- addParticipant("Raffaele")
  } yield thePeople

  // Initial settings
  val defaultSettings = Settings(
    colour       = "blue",
    count        = 0,
    participants = Seq()
  )

  // Run the pipeline providing the initial settings
  val (finalSettings: Settings, _) = finalSettingsState
    .run(defaultSettings)
    .value

  println(s"Final settings:\n  $finalSettings\n")
}

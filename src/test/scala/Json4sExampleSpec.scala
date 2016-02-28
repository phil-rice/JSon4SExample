import java.time.OffsetDateTime

import org.json4s.native.JsonMethods._
import org.json4s.{Extraction, Formats}
import org.json4s.native.Serialization
import org.scalatest.{Matchers, FlatSpec}

case class HasTime(o: OffsetDateTime)

class Json4sExampleSpec extends FlatSpec with Matchers{

  def roundTrip[X](original: X)(implicit formats: Formats, mf: Manifest[X]) ={
    val s = Serialization.writePretty(Extraction.decompose(original))
    parse(s).extract[X]
  }

  "OffsetDateTime" should " serialize / deserialze" in {
    val now = HasTime(OffsetDateTime.now)
    implicit val formats = Json4Adapter.formats()

    roundTrip(now) shouldBe now
  }

  

}

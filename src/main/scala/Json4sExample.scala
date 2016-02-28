import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter

import org.json4s.Extraction._
import org.json4s.JsonAST.JString
import org.json4s._
import org.json4s.native.Serialization

object Json4Adapter extends Json4Adapter {
 protected val dateTimeFormat = DateTimeFormatter.ISO_DATE_TIME

  implicit def offsetDateTimeToJObject(o: OffsetDateTime) = new JString(dateTimeFormat.format(o))

  object DateTimeSerializer extends CustomSerializer[OffsetDateTime](formats => ( {
    case JString(s) => OffsetDateTime.parse(s, dateTimeFormat)
  }, {
    case d: OffsetDateTime => JString(dateTimeFormat.format(d))
  }))


  implicit def toPimper(j: JValue) = new OurPimper(j)

  class OurPimper(j: JValue) {
    def ~(right: (String, JValue)): JValue = j match {
      case jo: JObject => JObject(jo.obj :+ JField(right._1, right._2))
    }
  }

  val serializers = List(DateTimeSerializer)

  def formats(x: Json4Adapter*) = (x :+ this).flatMap(_.serializers).foldLeft(DefaultFormats: Formats)((acc, cs) => acc + cs)
}


trait Json4Adapter {
  def serializers: List[CustomSerializer[_]]
}

case class Image(height: Int, width: Int, caption: Option[String])

case class PhotoGalleryImage(mainImage: Image, thumbnail: Image)

object PhotoGalleryImage extends Json4Adapter {

  import Json4Adapter._

  object PhotoGalleryImageCustomerSerializer extends CustomSerializer[PhotoGalleryImage](ser =
    implicit formats =>
      ( {
        case (jv: JValue) =>
          val image = jv.extract[Image]
          val thumbnail = (jv \ "thumbnail").extract[Image]
          PhotoGalleryImage(image, thumbnail)
      }, {
        case p: PhotoGalleryImage =>
          decompose(p.mainImage) ~ ("thumbnail" -> decompose(p.thumbnail))
      })) {
  }

  val serializers = List(PhotoGalleryImageCustomerSerializer)

}

object Index extends Json4Adapter {

  import JsonDSL._

  object IndexSerializer extends CustomSerializer[Index](
    ser = implicit formats => ( {
      case jv: JValue =>
        val id = (jv \ "id").extract[String]
        val field1 = (jv \ "field1").extract[String]
        val field2 = (jv \ "field2").extract[String]
        val media = jv \ "media"
        val offsetDateTime = (jv \ "offsetDateTime").extract[OffsetDateTime]
        val bodyImages = (media \ "bodyimages").extract[Map[String, Image]]
        val thumbnailImages = (media \ "thumbnails").extract[Map[String, Image]].values.toList
        val photoGallery = (media \ "photogallery").extract[Map[String, PhotoGalleryImage]]
        Index(id, field1, field2, offsetDateTime, bodyImages, thumbnailImages, photoGallery)
    }, {
      case i: Index =>
        import Json4Adapter.offsetDateTimeToJObject
        ("id" -> i.id) ~
          ("field1" -> i.field1) ~
          ("field2" -> i.field2) ~
          ("offsetDateTime" -> i.offsetDateTime) ~
          ("media" -> (
            ("bodyimages" -> Extraction.decompose(i.bodyImages)) ~
              ("thumbnails" -> Extraction.decompose(i.thumbnailImages.zipWithIndex.map { case (image, i) => (i.toString, image) }.toMap)) ~
              ("photogallery" -> Extraction.decompose(i.photoGallery))))
    }))

  val serializers = List(IndexSerializer)

}

case class Index(
                  id: String,
                  field1: String,
                  field2: String,
                  offsetDateTime: OffsetDateTime,
                  bodyImages: Map[String, Image],
                  thumbnailImages: List[Image],
                  photoGallery: Map[String, PhotoGalleryImage])


object Doit {


  def main(args: Array[String]) {
    import org.json4s.native.JsonMethods._
    implicit val formats = Json4Adapter.formats(PhotoGalleryImage, Index)

    val pg = PhotoGalleryImage(Image(10, 20, Some("main")), Image(30, 40, Some("thumbnail")))
    val image1 = Image(10, 20, Some("one"))
    val index = Index("someId", "some1", "some2", OffsetDateTime.now, Map("one" -> image1), List(image1), Map("xx" -> pg))

    val json = Extraction.decompose(pg)
    println(Serialization.write(json))
    println
    println
    println(Serialization.writePretty(Extraction.decompose(index)))
    println
    println

    val s = Serialization.writePretty(Extraction.decompose(index))
    val index2 = parse(s).extract[Index]

    println(Serialization.writePretty(Extraction.decompose(index2)))
  }

}

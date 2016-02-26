import org.json4s.native.Serialization


case class Image(height: Int, width: Int, caption: Option[String])

case class PhotoGalleryImage(mainImage: Image, thumbnail: Image)

case class Index(
                  id: String,
                  field1: String,
                  field2: String,
                  bodyImages: Map[String, Image],
                  thumbnailImages: List[Image],
                  photoGallery: Map[String, PhotoGalleryImage])


object Index {

  import org.json4s.JsonDSL._
  import org.json4s._
  import Extraction._

  implicit val formats = DefaultFormats + PhotoGalleryImageSerializer + IndexSerializer

  implicit def toPimper(j: JValue) = new OurPimper(j) {
  }

  class OurPimper(j: JValue) {
    def ~(right: (String, JValue)): JValue = j match {
      case jo: JObject => JObject(jo.obj :+ JField(right._1, right._2))
    }
  }

  def toJFieldList(x: JValue): List[JField] = x match {
    case j: JObject => j.obj
  }

  object PhotoGalleryImageSerializer extends CustomSerializer[PhotoGalleryImage](ser =
    implicit formats =>
      ( {
        case (jv: JValue) =>
          val image = jv.extract[Image]
          val thumbnail = (jv \ "thumbnailImage").extract[Image]
          PhotoGalleryImage(image, thumbnail)
      }, {
        case p: PhotoGalleryImage =>
          decompose(p.mainImage) ~ ("thumbnail" -> decompose(p.thumbnail))
      }))


  object IndexSerializer extends CustomSerializer[Index](
    ser = implicit formats => ( {
      case jv: JValue =>
        val id = (jv \ "id").extract[String]
        val field1 = (jv \ "field1").extract[String]
        val field2 = (jv \ "field2").extract[String]
        val media = jv \ "media"
        val bodyImages = (media \ "bodyimages").extract[Map[String, Image]]
        val thumbnailImages = (media \ "thumbnails").extract[List[Image]]
        val photoGallery = (media \ "photogallery").extract[Map[String, PhotoGalleryImage]]
        Index(id, field1, field2, bodyImages, thumbnailImages, photoGallery)
    }, {
      case i: Index =>
        ("id" -> i.id) ~
          ("field1" -> i.field1) ~
          ("field2" -> i.field2) ~
          ("media" -> (("bodyimages" -> Extraction.decompose(i.bodyImages)) ~
            ("thumbnails" -> Extraction.decompose(i.thumbnailImages)) ~
            ("photogallery" -> Extraction.decompose(i.photoGallery))))
    }))


  def main(args: Array[String]) {
    import org.json4s.native.JsonMethods._
    val pg = PhotoGalleryImage(Image(10, 20, Some("main")), Image(30, 40, Some("thumbnail")))
    val image1 = Image(10, 20, Some("one"))
    val index = Index("someId", "some1", "some2", Map("one" -> image1), List(image1), Map("xx" -> pg))

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

import org.json4s._
import org.json4s.JsonAST.JValue

class Id(val id: String) extends AnyVal

case class Image(height: Int, width: Int, caption: Option[String])

case class PhotoGalleryImage(mainImage: Image, thumbnail: Image)

case class Index(
                  id: String,
                  field1: String,
                  field2: String,
                  bodyImages: Map[Id, Image],
                  thumbnailImages: List[Image],
                  photoGallery: Map[Id, PhotoGalleryImage])


object Index {

  import org.json4s._
  import org.json4s.JsonDSL._
  import Extraction._


  object PhotoGalleryImageSerializer extends CustomSerializer[PhotoGalleryImage](ser =
    formats =>
      ( {
        case (jv: JValue) => val image = jv.extract[Image]
          val thumbnail = (jv \\ "thumbnail").extract[Image]
          PhotoGalleryImage(image, thumbnail)
      }, {
        case p: PhotoGalleryImage =>
          def toJFieldList(x: JValue): List[JField] = {
            case j: JObject => j.obj
          }
          val mainImageFields = toJFieldList(decompose(p.mainImage))
          val thumbNail = JField("thumbnail", decompose(p.thumbnail))
          JObject((mainImageFields :+ thumbNail): _*)
      }))


  object IndexSerializer extends CustomSerializer[Index](
    ser = formats => ( {
      case jv: JValue =>
        val id = (jv \ "id").extract[String]
        val field1 = (jv \ "id").extract[String]
        val field2 = (jv \ "id").extract[String]
        val bodyImages = (jv \ "id").extract[Map[Id, Image]]
        val thumbnailImages = (jv \ "thing").extract[List[Image]]
        val photoGallery = (jv \ "thing").extract[Map[Id, PhotoGalleryImage]]
        Index(id, field1, field2, bodyImages, thumbnailImages, photoGallery)
    }, {
      case i: Index =>
        decompose()
        ("id" -> i.id) ~
          ("field1" -> i.field1) ~
          ("field2" -> i.field2) ~
          ("media" -> decompose(Map("body_images" -> decompose(i.bodyImages)))

    }))

}

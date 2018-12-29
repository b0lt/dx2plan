package dx2plan

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobal, JSImport}

@js.native
@JSGlobal
object LZMA extends js.Object {
  def compress(bytes: js.Array[Byte], mode: Int): js.Array[Byte] = js.native
  def decompress(bytes: js.Array[Byte]): js.Array[Byte] = js.native
}

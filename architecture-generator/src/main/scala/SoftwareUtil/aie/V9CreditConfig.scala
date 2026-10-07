package SoftwareUtil.aie
import java.io.{File, PrintWriter}
/** V9 emits descriptor constants and helpers, never application wrapper sources. */
object V9CreditConfig {
  case class Receive(input: Int, output: Int, words: Int, returnWords: Int,
      tag: Int, taggedOutput: Boolean, stream: Boolean)
  case class Encode(output: Int, words: Int)
  case class Kernel(unit: String, function: String, inputs: Seq[String], outputs: Seq[String],
      receives: Seq[Receive], encodes: Seq[Encode])
  def write(projectFolder: String, kernels: Seq[Kernel]): Unit = {
    val folder = new File(projectFolder, "kernels"); folder.mkdirs()
    def save(name: String, text: String): Unit = {
      val writer = new PrintWriter(new File(folder, name))
      try writer.write(text) finally writer.close()
    }
    val lines = Seq("#pragma once", "#include \"hardcilk_credit.h\"", "namespace hc_config {") ++
      kernels.groupBy(_.function).toSeq.sortBy(_._1).flatMap { case (function, instances) =>
        val signatures = instances.map(k => (k.inputs, k.outputs, k.receives, k.encodes)).distinct
        require(signatures.size == 1,
          s"V9 function '$function' has incompatible per-instance credit layouts; use separate application function names for different layouts")
        val k = instances.head
        k.inputs.zipWithIndex.map { case (port, index) =>
          val params = k.receives.find(_.input == index).map(r =>
            s"true, ${r.words}, ${r.returnWords}, ${r.tag}, ${r.taggedOutput}, ${r.stream}, ${r.output}")
            .getOrElse("false, 1, 0, 0, false, false, 0")
          s"using ${function}_${port} = hc::Route<$params>;"
        } ++ k.outputs.zipWithIndex.map { case (port, index) =>
          val words = k.encodes.find(_.output == index).map(_.words).getOrElse(0)
          s"using ${function}_out$index = hc::Output<$words>;"
        }
      } ++ Seq("}")
    save("hardcilk_credit_config.h", lines.mkString("\n") + "\n")
    save("hardcilk_credit.h", runtime)
  }
  val runtime: String = """#pragma once
#include <adf.h>
#include <cstdint>
namespace hc {
inline void fence() {
#ifdef __chess__
  chess_separator_scheduler(1);
#endif
}
template<bool Enabled, unsigned Words, unsigned ReturnWords, unsigned Tag, bool Tagged, bool Stream, unsigned OutputIndex>
struct Route {
  static constexpr bool enabled=Enabled, stream=Stream, tagged=Tagged;
  static constexpr unsigned words=Words, return_words=ReturnWords, tag=Tag, output_index=OutputIndex;
};
// Application calls this after consuming one complete receive unit.
template<class C, unsigned N> void return_credit(output_stream_uint32 *const (&outputs)[N]) {
  static_assert(!C::enabled || C::output_index<N, "Credit return port does not exist");
  if (!C::enabled) return;
  output_stream_uint32 *out=outputs[C::output_index];
  fence();
  for (unsigned i=0; i<C::return_words; ++i) {
    const uint32_t word=C::tagged ? (i==C::return_words-1 ? C::tag : 0u)
        : (i==0 ? C::tag : (i==1 ? 0xffffc5edu : 0u));
    ::writeincr(out, word, i==C::return_words-1);
  }
  fence();
}
template<unsigned Words> struct Output { static constexpr unsigned words=Words; };
template<class C> bool payload_last(bool last) { return C::words ? false : last; }
// Single data route multiplexed with credits: append the zero data tag/padding.
// Existing multi-route tagged application outputs already carry their own tags.
template<class C> void finish_record(output_stream_uint32 *out) {
  if (C::words) for (unsigned i=0; i<4; ++i) ::writeincr(out, 0u, i==3);
}

}
"""
}

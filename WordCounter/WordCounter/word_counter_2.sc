class multivariavel {
  var data_string: String = ""
  var data_array: Array[String] = Array.empty[String]
  var data_list: List[String] = List.empty[String]
  var data_seq: Seq[(String, Int)] = Seq.empty[(String, Int)]
}

trait Step {
  def process(data:multivariavel):multivariavel
  def run(data:multivariavel, next:Step):multivariavel
}

abstract class MidStep (next2:Step) extends Step {
  def run(data:multivariavel, next:Step):multivariavel = next.run(process(data),next2)
}

abstract class EndStep extends Step {
  def run(data:multivariavel, next:Step):multivariavel = process(data)
}

object no_op extends EndStep {
  def process(data:multivariavel):multivariavel = null
}

val s10 = null
object print_text extends MidStep (s10) {
  def process(data:multivariavel):multivariavel = {
    for (w <- data.data_seq) println(w._1 + " - " + w._2)
    return data
  }
}

val s9 = no_op
object sort extends MidStep (s9) {
  def process(data:multivariavel):multivariavel = {
    data.data_seq = data.data_seq.sortWith(_._2 > _._2)
    return data
  }
}

val s8 = print_text
object frequencies extends MidStep (s8) {
  def process(data:multivariavel):multivariavel = {
    data.data_seq = data.data_list.groupBy(identity).mapValues(_.size).toSeq
    return data
  }
}

val s7 = sort
object remove_stop_words extends MidStep (s7) {
  def process(data:multivariavel):multivariavel = {

    var stop_words = scala.io.Source.fromFile("C:\\Users\\Felipe\\Desktop\\WordCounter\\stop_words.txt").mkString
    stop_words = stop_words.replaceAll("[^A-Za-z0-9 ]", " ")
    val stop_words_list = stop_words.split(" +")

    for (i <- data.data_array) {
      var keep = true
      for (j <- stop_words_list) {
        if (i == j) {
          keep = false
        }
      }
      if (keep == true){
        data.data_list = data.data_list ++ List(i)
      }
    }

    return data
  }
}

val s6 = frequencies
object scan extends MidStep (s6) {
  def process(data:multivariavel):multivariavel = {
    data.data_array = data.data_string.split(" +")
    return data
  }
}

val s5 = remove_stop_words
object normalize extends MidStep (s5) {
  def process(data:multivariavel):multivariavel = {
    data.data_string = data.data_string.toLowerCase()
    return data
  }
}

val s4 = scan
object filter_chars extends MidStep (s4) {
  def process(data:multivariavel):multivariavel = {
    data.data_string = data.data_string.replaceAll("[^A-Za-z0-9 ]", " ")
    return data
  }
}

val s3 = normalize
object read_file extends MidStep (s3) {
  def process(data:multivariavel):multivariavel = {
    data.data_string = scala.io.Source.fromFile(data.data_string).mkString
    return data
  }
}

var entrada = new multivariavel
entrada.data_string = "C:\\Users\\Felipe\\Desktop\\WordCounter\\text.txt"
println(read_file.run(entrada,filter_chars))
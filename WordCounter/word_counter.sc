// no_operation / Simboliza o final da cadeia do kick forward
def no_op(func: () => Unit): Unit = {
  println("Encerrando o programa")
  return
}

// Printa os elementos de uma sequência de (String, Int)
def print_text(data: Seq[(String, Int)], func: (() => Unit) => Unit): Unit = {
  println("Printando o resultado")
  for (w <- data) println(w._1 + " - " + w._2)
  func(null)
}

// Ordena a sequência de (String, Int) pelo 2º elemento
def sort(data: Seq[(String, Int)], func: (Seq[(String, Int)], (() => Unit) => Unit) => Unit): Unit = {
  println("Executando sort")
  func(data.sortWith(_._2 > _._2), no_op)
}

// Mapeia a frequência com que cada termo aparece na lista
// Gera uma sequência com estruturas da foram (String, Int), em que o 1º é um
// elemento da lista, e o 2º é quantas vezes esse elemento aparece.
def frequencies(data: List[String], func: (Seq[(String, Int)], (Seq[(String, Int)], (() => Unit) => Unit) => Unit) => Unit): Unit = {
  println("Executando frequencies")
  func(data.groupBy(identity).mapValues(_.size).toSeq, print_text)
}

// Cria uma Sequência de strings, que tem todos os elementos do array de strings
// recebido, com exceção daqueles que estão no stop_words.txt.
def remove_stop_words(data: Array[String], func: (List[String], (Seq[(String, Int)], (Seq[(String, Int)], (() => Unit) => Unit) => Unit) => Unit) => Unit): Unit = {
  println("Executando remove_stop_words")
  var stop_words = scala.io.Source.fromFile("C:\\Users\\Família\\Desktop\\TP2-kickforward-main\\TP2-kickforward-main\\WordCounter\\top_words.txt").mkString
  stop_words = stop_words.replaceAll("[^A-Za-z0-9 ]", " ")
  val stop_words_list = stop_words.split(" +")
  var new_data = List.empty[String]

  for (i <- data) {
    var keep = true
    for (j <- stop_words_list) {
      if (i == j) {
        keep = false
      }
    }
    if (keep == true){
      new_data = new_data ++ List(i)
    }
  }

  func(new_data, sort)
}

// Separa a string recebida em palavras, delimitando cada uma nos espaços.
// Note que, com o uso da expressão regular " +", qualquer número arbitrário de
// espaços consecutivos ainda é considerado como um único espaço para efeitos de
// separação de palavras.
def scan(data: String, func: (Array[String], (List[String], (Seq[(String, Int)], (Seq[(String, Int)], (() => Unit) => Unit) => Unit) => Unit) => Unit) => Unit): Unit = {
  println("Executando scan")
  val word_list = data.split(" +")
  func(word_list, frequencies)
}

// Coloca todas as letras da string recebida em minúsculo
def normalize(data: String, func: (String, (Array[String], (List[String], (Seq[(String, Int)], (Seq[(String, Int)], (() => Unit) => Unit) => Unit) => Unit) => Unit) => Unit) => Unit): Unit = {
  println("Executando normalize")
  func(data.toLowerCase(), remove_stop_words)
}

// Retira todos os caracteres não-alfanuméricos da string recebida
def filter_chars(data: String, func: (String, (String, (Array[String], (List[String], (Seq[(String, Int)], (Seq[(String, Int)], (() => Unit) => Unit) => Unit) => Unit) => Unit) => Unit) => Unit) => Unit): Unit = {
  println("Executando filter_chars")
  func(data.replaceAll("[^A-Za-z0-9 ]", " "), scan)
}

// Lê o conteúdo do arquivo especificado no path
def read_file(path: String, func: (String, (String, (String, (Array[String], (List[String], (Seq[(String, Int)], (Seq[(String, Int)], (() => Unit) => Unit) => Unit) => Unit) => Unit) => Unit) => Unit) => Unit) => Unit): Unit = {
  println("Executando read_file")
  func(scala.io.Source.fromFile(path).mkString, normalize)
}

println("Iniciando o programa")
read_file("C:\\Users\\Família\\Desktop\\TP2-kickforward-main\\TP2-kickforward-main\\WordCounter\\text.txt", filter_chars)

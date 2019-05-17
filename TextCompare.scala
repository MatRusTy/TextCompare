import scala.math._
// Author: MatRusTy - https://github.com/MatRusTy
// Assuming: labels != label and Classification = classification.
// That is, we don't stem, but we ignore upper/lowercase differences.
object textCompare{
    def main(args: Array[String]): Unit = {
        val text1 = "Unlike classification or prediction, which analyzes data objects with class labels, clustering analyzes data objects without consulting a known class label."
        val text2 = "Classification can be used for prediction of class labels of data objects. However, in many applications, prediction of missing values is performed to fit data objects into a schema."
        val text3 = "Sun Salutation, a ritual performed in the early morning, combines seven different postures. The sun, the life generator, is invoked by this Yogic exercise."
        val stopwords = List("a", "an", "are", "be", "because", "by", "can", "for", "however", "in", "into", "is", "keep", "many", "not", "of", "or", "rather", "than", "the", "they", "this", "to", "unlike", "used", "way", "which", "with", "without")
        println("-------------------------- text1 - text2 --------------------------")
        compare(text1, text2, stopwords)
        println("-------------------------- text1 - text2 --------------------------")
        println("")
        println("-------------------------- text1 - text3 --------------------------")
        compare(text1, text3, stopwords)
        println("-------------------------- text1 - text3 --------------------------")
        println("")
        println("-------------------------- text2 - text3 --------------------------")
        compare(text2, text3, stopwords)
        println("-------------------------- text2 - text3 --------------------------")
    }

    def compare(text1: String, text2: String, stopwords: List[String]): Unit = {
        val t1 = text1.toLowerCase().filterNot(p => p == ',' || p == '.').split(" ").toList.filter(p => !(stopwords.contains(p)))
        val t2 = text2.toLowerCase().filterNot(p => p == ',' || p == '.').split(" ").toList.filter(p => !(stopwords.contains(p)))
        val wordSet = (t1 ++ t2).distinct
        val t1vector: List[Int] = wordSet.map(s => t1.count(p => s.equals(p)))
        val t2vector: List[Int] = wordSet.map(s => t2.count(p => s.equals(p)))
        println(s"Formatted text 1: ${t1.mkString(" ")}")
        println(s"Formatted text 2: ${t2.mkString(" ")}")
        println(s"Set of words: ${wordSet.mkString("{",", ", "}")}")
        println(s"Text 1 Vector: ${t1vector.mkString("[",", ","]")}")
        println(s"Text 2 Vector: ${t2vector.mkString("[",", ","]")}")
        val euclideanDistance = d(t1vector, t2vector)
        println(s"Euclidean distance: $euclideanDistance")
        val cosineSimilarity = c(t1vector, t2vector)
        println(s"Cosine similarity:  $cosineSimilarity")
    }

    def d(v1: List[Int], v2: List[Int]): Double = {
        if(v1.size != v2.size) throw new Exception("Vectors have different dimensions")
        val pairs = v1.zip(v2)
        val difSquaredSum: Double = pairs.map({case (p,q) => math.pow(p - q, 2)}).sum
        math.sqrt(difSquaredSum)
    }

    def c(v1: List[Int], v2: List[Int]): Double = {
        if(v1.size != v2.size) throw new Exception("Vectors have different dimensions")
        val dotproduct = v1.zip(v2).map({case (p,q) => p * q}).sum
        val v1Length = math.sqrt(v1.map(p => math.pow(p, 2)).sum)
        val v2Length = math.sqrt(v2.map(p => math.pow(p, 2)).sum)
        dotproduct / (v1Length * v2Length)
    }
}
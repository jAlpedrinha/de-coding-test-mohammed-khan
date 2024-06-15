package ai.humn.telematics


import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.math._

object ProcessDataFile {
  //Earth radious in kilometers
  val EarthRadius =6371.0

  def main(args: Array[String]) = {

    // read file path from args
    var x = args(0)

    // This is the file
    val y = Source.fromFile(x)
    //y.getLines().drop(1)

    // All of the lines in the file
    var l: Seq[String] = y.getLines().drop(1).toList //This is for removing Header alone

    // Make a variable to hold the parsed lines from the file.
    var results = ListBuffer[Array[String]]()

    // parse each line as csv to a collection
    for (a <- 0 until l.length) {
      results += l(a).split(",")
    }

    // This is a collection of the journey lines
    val j = results.toSet.toList

    // 1. Find journeys that are 90 minutes or more.
    println("Journeys of 90 minutes or more.")
    var i = 0
    var ok = true
    while (ok) {
      if (j(i)(2).toLong - j(i)(3).toLong >= 90 * 1000 * 60) {
        println("This journey took longer than 90 minutes")
        println(j(i).mkString(","))
      }
      i = i + 1
      if (i >= j.size) ok = false
    }

    // need to do the
    // 2. Find the average speed per journey in kph.
    println("Average speeds in Kph.")
    i = 0
    ok = true
    while (ok) {
      var speed = calculateSpeed(j(i)(4).toDouble, j(i)(5).toDouble, j(i)(2).toLong, j(i)(6).toDouble, j(i)(7).toDouble, j(i)(3).toLong)
      println(j(i).mkString(",") + " avgSpeed in kph was " + speed)
      i = i + 1
      if (i >= j.size) ok = false
    }

    // her eis where I will
    // 3. Find the total mileage by driver for the whole day.
    println("Mileage By Driver.")
    case class Mileage(DriverID:String,startReading:Int,endReading:Int)

    //calculate mileage of each driver
    def calculateMileage(reading:Seq[Mileage]): Int = {
      reading.map(x => x.endReading - x.startReading).sum
    }

    //extract the driver id ,start startOdometer and end startOdometer
    def driverReading(record:String):Mileage={
      val fileds = record.split(",")
      Mileage(fileds(1).trim,fileds(8).trim.toInt,fileds(9).trim.toInt)
    }

    val driverData = l.distinct.map(driverReading)
    val groupData = driverData.groupBy(_.DriverID)
    val mileageByDriver = groupData.mapValues(calculateMileage).filter({case (_, value) => value >0})
    mileageByDriver.foreach({case (key,value)=>
      println(key+" drove " + value +" kilometers")})


    // This part is the last part of the puzzle
    // This jira was a little bit unclear.
    // I assume that most active driver means the driver who drove the most mileage
    // for all of the journeys.
    // we somehow need to
    // 4. Find the most active driver - the driver who has driven the most kilometers.
    println("Most active driver...")
    println("Most active driver is "+mileageByDriver.maxBy(_._2)._1)

  }
  //Calculate average speed given two latitude and longitude point ans time difference in seconds
  def calculateSpeed(lat1:Double,lon1:Double,time1:Long,lat2:Double,lon2:Double,time2:Long):Double={
    val distance = haversine(lat1,lon1,lat2,lon2)
    val timeDiff = (time2/1000.0-time1/1000.0).toDouble /3600
    val speed = math.max(distance / timeDiff,0)
    speed
  }
  //Haversine formula to calculate distance between two latitude and longitude points
  def haversine(lat1:Double,lon1:Double,lat2:Double,lon2:Double):Double={
    val dLat = toRadians(lat2 - lat1)
    val dLon = toRadians(lon2 - lon1)
    val a = pow(sin(dLat /2),2) + cos(toRadians(lat1)) * cos(toRadians(lat2)) * pow(sin(dLon/2),2)
    val c = 2 * atan2(sqrt(a),sqrt(1-a))
    EarthRadius * c
  }
  //Convert degree to radians
  def toRadians(degree: Double):Double={
    degree * (Pi /100)
  }

}

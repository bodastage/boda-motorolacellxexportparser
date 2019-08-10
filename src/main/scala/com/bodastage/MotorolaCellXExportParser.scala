package com.bodastage

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Path, Paths}
import scopt.OParser
import scala.io.Source
import util.control.Breaks._
import scala.collection.mutable.ArrayBuffer

case class Config(
                   in: File = new File("."),
                   out: File = null,
                   version : Boolean = false
                 )
object MotorolaCellXExportParser {
  var outputFolder: String = "";

  def main(args: Array[String]): Unit = {

    val builder = OParser.builder[Config]
    val parser1 = {
      import builder._
      OParser.sequence(
        programName("boda-motorolacellxexportparser"),
        head("boda-motorolacellxexportparser", "0.0.2"),
        opt[File]('i', "in")
          .required()
          .valueName("<file>")
          .action((x, c) => c.copy(in = x))
          .validate(f =>
            if( (!Files.isRegularFile(f.toPath) && !Files.isDirectory(f.toPath))
              && !Files.isReadable(f.toPath)) failure(s"Failed to access input file/directory called ${f.getName}")
            else success
          )
          .text("input file or directory, required."),
        opt[File]('o', "out")
          .valueName("<file>")
          .action((x, c) => c.copy(out = x))
          .validate(f =>
            if( !Files.isDirectory(f.toPath ) && !Files.isReadable(f.toPath)) failure(s"Failed to access outputdirectory called ${f.getName}")
            else success
          )
          .text("output directory required."),
        opt[Unit]('v', "version")
          .action((_, c) => c.copy(version = true))
          .text("Show version"),
        help("help").text("prints this usage text"),
        note(sys.props("line.separator")),
        note("Parses Nokia performance management files to csv. It processes plain text XML and gzipped XML files."),
        note("Examples:"),
        note("java -jar boda-motorolacellxexportparser.jar -i FILENAME.xml"),
        note("java -jar boda-motorolacellxexportparser.jar -i FILENAME.gz"),
        note("java -jar boda-motorolacellxexportparser.jar -i FILENAME.xml -o /path/to/folder"),
        note("java -jar boda-motorolacellxexportparser.jar -i FILENAME.gz -o /path/to/folder"),
        note(sys.props("line.separator")),
        note("Copyright (c) 2019 Bodastage Solutions(http://www.bodastage.com)")

      )
    }

    var inputFile : String = ""
    var outFile : File = null;
    var showVersion : Boolean = false;
    OParser.parse(parser1, args, Config()) match {
      case Some(config) =>
        inputFile = config.in.getAbsolutePath
        outFile = config.out
        showVersion = config.version
      case _ =>
        // arguments are bad, error message will have been displayed
        sys.exit(1)
    }

    try{

      if(showVersion){
        println("0.1.0")
        sys.exit(0);
      }

      if(outFile != null) outputFolder = outFile.getAbsoluteFile().toString


      this.processFileOrDirectory(inputFile)

    }catch{
      case ex: Exception => {
        println("Error accessing file")
        sys.exit(1)
      }
    }

  }

  /**
    * Get file base name
    *
    * @param fileName
    * @return
    */
  def getFileBaseName(fileName: String): String ={
    try{
      return new File(fileName).getName
    }catch{
      case ex: Exception => {
        return fileName
      }
    }
  }


  def processFileOrDirectory(inputPath: String): Unit ={


    val file : Path = Paths.get(inputPath)
    val isRegularExecutableFile : Boolean = Files.isRegularFile(file) & Files.isReadable(file)
    val isReadableDirectory = Files.isDirectory(file) & Files.isReadable(file)

    if (isRegularExecutableFile) {
      this.parseFile(inputPath)
    }

    if (isReadableDirectory) {
      val directory = new File(inputPath)

      val fList = directory.listFiles
      for(f:File <- fList){
        try {
          if( Files.isRegularFile(f.toPath)){
            this.parseFile(f.getAbsolutePath)
          }else{
            println(s"${f.getAbsolutePath} is not a regular file. Skipping it.")
          }

        }catch {
          case ex: Exception => {
            println(s"Error processing ${f.getAbsolutePath}")
          }
        }
      }
    }
  }

  /**
    * Parse a file
    * @param filename
    */
  def parseFile(fileName: String) : Unit = {
    val fileBaseName: String  = getFileBaseName(fileName);
    var pw : PrintWriter = null;
    //var parameters = ArrayBuffer[String]()
    var parameters : Array[String] = Array[String]()

    if(outputFolder.length > 0) {
      val csvFile: String = outputFolder + File.separator + fileBaseName + ".csv";
      pw = new PrintWriter(new File(csvFile));
    }
    var lineCount : Integer = 0;
    var header = "";
    var headerArray : Array[String] =  Array[String]()

    for (line <- Source.fromFile(fileName).getLines) {
      lineCount += 1;
      var rowArray : Array[String] = line.split("\t")

      breakable {
        if(lineCount == 1){
          headerArray = rowArray.map(v => v.replaceAll("\\s+", "_").toLowerCase())

          for(i <- 0 to headerArray.length-1){
            if( !(parameters contains(headerArray(i))) ){
              parameters = parameters :+  headerArray(i)
            }
          }

          if(outputFolder.length == 0) {
            println(parameters.mkString(","));
          }else{
            pw.write(parameters.mkString(",")+ "\n");
          }

          break;
        }

        //Eeach position corresponds to a parameter in parameters array
        var csvRowValues : Array[String] = new Array[String](parameters.length);
        csvRowValues = csvRowValues.map(v => "")

        for(i <- 0 to rowArray.length - 1){
          val value = rowArray(i)

          //Get parameter name at that position
          val param = headerArray(i)

          //Get index in parameters
          val paramIndex = parameters.indexWhere( _ == param)

          //Set vaue
          if(csvRowValues(paramIndex).length > 0 ){
            csvRowValues(paramIndex) = csvRowValues(paramIndex) + ";" + value;
          }else{
            csvRowValues(paramIndex) = value;
          }

        }

       val  csvRow = csvRowValues.map(v => toCSVFormat(v)).mkString(",")

        if(outputFolder.length == 0) {
          println(csvRow);
        }else{
          pw.write(csvRow + "\n");
        }

      }
    }

    if(pw != null ) pw.close();

  }

  def toCSVFormat(s: String): String = {
    var csvValue: String = s

    if(s.matches(".*,.*")){
      csvValue = "\"" + s + "\""
    }

    if(s.matches(""".*".*""")){
      csvValue = "\"" + s.replace("\"", "\"\"") + "\""
    }

    return csvValue
  }

}

package com.bodastage;

import org.scalatest._

import scala.io.Source
import java.io.File

class MotorolaCellXExportParserSpec extends FunSuite {
    test("MotorolaCellXExportParser"){
        val resourcePath = getClass.getResource("/RFPlanCell.out")
        var inputFile : String = resourcePath.getPath
        val outDir = System.getProperty("java.io.tmpdir")

        var parser = MotorolaCellXExportParser;
        var args : Array[String] = Array("-i", inputFile, "-o", outDir);
        parser.main(args);
		val dateTime : String = parser.getDateTime();

        val expectedCSV = s"filename,datetime,msc_name,omc_name,bss_name,site_name\nRFPlanCell.out,${dateTime},YYY,ABCDEFG,TT_00_4W,SiteName";

        val sourceCSV = Source.fromFile(outDir + File.separator + "RFPlanCell.out.csv").getLines().mkString("\n");
        assert(expectedCSV == sourceCSV)
    }
}

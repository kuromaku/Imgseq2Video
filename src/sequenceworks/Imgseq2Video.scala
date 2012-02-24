package sequenceworks
import swing._
import event._
import java.io.File
import java.io.FileOutputStream
import java.io.FileInputStream
import java.io.FileWriter
import scala.io.Source
import javax.swing.filechooser.FileNameExtensionFilter
import javax.swing.filechooser.FileFilter

import javax.swing.JFileChooser
import javax.swing.border.Border
import javax.swing.BorderFactory
import javax.imageio.ImageIO

import scala.collection.mutable.LinkedList
import scala.collection.immutable.ListSet
import scala.math.Ordering.StringOrdering
import scala.math.Ordering.String
import scala.math.Ordering
import scala.util.matching.Regex
import scala.swing.FileChooser.SelectionMode
import scala.xml._

object Imgseq2Video extends SimpleSwingApplication {
  var saveDirectory = "/home/"
  var movieDirectory = "/home/"
  var tmpDirectory = "/home/"
  var singleMovieName = "newmovie"
  val textArea = new TextArea("No files selected yet...\n")
  textArea.border_=(BorderFactory.createTitledBorder(BorderFactory.createLineBorder(new Color(155,155,155)),"Images:"))
  var selFiles: Seq[File] = new LinkedList[File]()
  var min1:Int = 0
  var max1:Int = 0
  var pfix = "x"
  var loopDone = false
  var bPoints = new LinkedList[Int]()
  var normalized = false
  var reversed = false
  var manyFiles = false
  var concatenatedMovie = false
  var loopNotName = false
  var properSequences = false
  val infoArea = new TextArea("Load a sequence of image files to get started\n")
  infoArea.border_=(BorderFactory.createTitledBorder(BorderFactory.createLineBorder(new Color(155,155,155)),"Program messages:"))
  val regexpr = new Regex("\\w+\\_\\d{4}\\.\\w+")
  var properlyIndexedSequences = false
  var frameRate = 25
  var givenName = "imageseries"
  readConfig("videoBuilderConfig.xml")
  def copyImage(src:String,tgt:String) : Boolean = {
    val fin = new FileInputStream(src)
    val fout = new FileOutputStream(tgt)
    var bytes = new Array[Byte](512)
    var bytesRead = fin.read(bytes)
    while (bytesRead > 0) {
      fout.write(bytes,0,bytesRead)
      bytesRead = fin.read(bytes)
    }
    fin.close
    fout.close
    return true
  }
  def copyImage2(src:String,tgt:String) : Boolean = {
    ImageIO.write(ImageIO.read(new File(src)),"jpg",new File(tgt))
  }
  def top = new MainFrame {
    title = "Imgseq2Video"
    location_=(new Point(100,100))
    object fchooser extends FileChooser
    object openFiles extends Button { text_=("OpenOneSequence") }
    object openMany extends Button { text_=("OpenManySequences") }
    object loopNow extends Button { text_=("Make loop"); enabled_=(false) }
    object normalizeNow extends Button { text_=("Normalize sequence"); enabled_=(false) }
    object dirField extends TextField("Saving files to: "+saveDirectory) { }
    object makeMovie extends Button { text_=("Make Movie"); enabled_=(false) }
    object reverseSequence extends Button { text_=("Reverse"); enabled_=(false) }
    object normalizeSeq extends MenuItem("Normalize sequence") { enabled_=(false) }
    //object createDialog extends MenuItem("Create Dialog") { enabled_=(true) }
    object concatenateMovies extends MenuItem("Create a single movie") { enabled_=(false) }
    object saveConfig extends MenuItem("Save Configuration") { enabled_=(true) }
    object infoLauncher extends MenuItem("About") { }
    object closeApp extends MenuItem("Exit") { }
    //object checkAllFiles extends MenuItem("FileCheck") { }
    object clearAll extends Button { text_=("Clear all") }
    object saveLabel extends Label { text_=(saveDirectory) }
    object movieDirLabel extends Label { text_=(movieDirectory) }
    object tmpLabel extends Label { text_=(tmpDirectory) }
    object saveDir extends Button { text_=("SaveDir") }
    object movDir extends Button { text_=("MovieDir") }
    object tmpDir extends Button { text_=("TmpDir") }
    object cancelButton extends Button { text_=("Cancel") }
    object proceedButton extends Button { text_=("Proceed") }
    object dialWindow extends Dialog {}
    object fileNameDialog extends Dialog {}
    object nameField extends TextField { text_=(singleMovieName) }
    object loopSlider extends Slider { min_=(2); max_=(20); paintTicks_=(true); value_=(2); name_=("Number of Loops") }
    object sliderLabel extends Label { text_=("Loops=2") }
    object frameRateLabel extends Label { text_=("Frame Rate=25") }
    object frameRateSlider extends Slider { min_=(1); max_=(50); paintTicks_=(true); value_=(frameRate) }
    val filter = new FileNameExtensionFilter("jpg images","jpg","jpeg")
    //var imgSeqsLoaded = 0
    var imagesLoaded = false
    
    //menuAction.
    val mbar = new MenuBar {
      contents += new Menu("Actions") {
        contents += normalizeSeq
        contents += concatenateMovies
        //contents += checkAllFiles
        //contents += createDialog
        contents += saveConfig
        contents += closeApp
      }
      contents += new Menu("Help") {
        contents += infoLauncher
      }
    }
    menuBar_=(mbar)
    contents = new GridPanel(2,1) {
      val fpane = new GridPanel(1,2) {
        val gpane = new GridPanel(7,1) {
          contents += openFiles
          contents += openMany
          contents += loopNow
          contents += normalizeNow
          contents += reverseSequence
          contents += makeMovie
          contents += clearAll
        }
        contents += gpane
        //contents += new ScrollPane(infoArea)
        val locPanel = new GridPanel(7,1) {
          contents += saveDir
          contents += saveLabel
          contents += movDir
          contents += movieDirLabel
          contents += tmpDir
          contents += tmpLabel
          val ratePane = new GridPanel(1,2) {
            contents += frameRateLabel
            contents += frameRateSlider
          }
          contents += ratePane
        }
        contents += locPanel
      }
      
      val txtpanels = new GridPanel(2,1) {
        contents += new ScrollPane(infoArea)
        contents += new ScrollPane(textArea)
      }
      contents += fpane
      contents += txtpanels
      
    }
    listenTo(openFiles,openMany,loopNow,makeMovie,
        normalizeSeq,concatenateMovies,clearAll,closeApp,
        normalizeNow,reverseSequence,saveConfig,saveDir,movDir,tmpDir,
        cancelButton,proceedButton,loopSlider,nameField,frameRateSlider,infoLauncher)
    reactions += {
      case ButtonClicked(`openFiles`) => {
        val chooser = new FileChooser
        chooser.multiSelectionEnabled_=(true)
        chooser.fileFilter_=(filter)
        chooser.showOpenDialog(contents.first)
        selFiles = chooser.selectedFiles
        var ((min,max),l) = processFiles(selFiles)
        min1 = min
        max1 = max
        bPoints = l
        if (selFiles.length > 1) {
          properlyIndexedSequences = fileNameChecker(selFiles)
          imagesLoaded = true
          if (!properlyIndexedSequences) {
            infoArea.append("The files don't appear to be properly indexed (like image_0001.jpg to image_0100.jpg)\n")
          }
          if (min1 > 1) {
            normalizeSeq.enabled_=(true)
            normalizeNow.enabled_=(true)
            infoArea.append("Loaded "+selFiles.length+" images\nYou can now normalize the image indexes before creating a movie.")
          }
          else {
            loopNow.enabled_=(true)
            makeMovie.enabled_=(true)
            reverseSequence.enabled_=(true)
            infoArea.append("Loaded "+selFiles.length+" images\n Please select a further action.")
          }
          openFiles.enabled_=(false)
        }
        textArea.text_=("")
        for (f <- selFiles) {
          textArea.append(f.getPath+"\n")
        }
        centerOnScreen
      }
      case ButtonClicked(`openMany`) => {
        val chooser = new FileChooser
        chooser.multiSelectionEnabled_=(true)
        chooser.fileSelectionMode_=(SelectionMode.FilesAndDirectories)
        //chooser.fileFilter_=(filter)
        val reval = chooser.showDialog(contents.first,"Open jpg images or a directory")
        if (reval.toString.equals("Approve") && chooser.selectedFiles.first.isDirectory()) {
          if (imagesLoaded) {
            selFiles ++ getImages(chooser.selectedFiles.first)
          }
          else {
            selFiles = getImages(chooser.selectedFiles.first)
          }
        }
        else if (reval.toString == "Approve"){
          if (imagesLoaded) {
            selFiles ++ chooser.selectedFiles
          }
          else {
            selFiles = chooser.selectedFiles
          }

        }
        if (selFiles.length > 1) {
          properlyIndexedSequences = fileNameChecker(selFiles)
          if (!properlyIndexedSequences) {
            infoArea.append("The files don't appear to be properly indexed (like image_0001.jpg to image_0100.jpg)\n")
          }
          makeMovie.enabled_=(true)
          concatenateMovies.enabled_=(true)
          manyFiles = true
          textArea.text_=("")
          for (f <- selFiles) {
            textArea.append(f.getPath+"\n")
          }
        }

        
        
      }
      case ButtonClicked(`loopNow`) => {
        val dialPane = new GridPanel(2,2) {
          contents += sliderLabel
          contents += loopSlider
          contents += cancelButton
          contents += proceedButton
        }
        loopNotName = true
        val dialog = dialWindow
        dialog.contents_=(dialPane)
        dialog.visible_=(true)

      }
      case ValueChanged(`loopSlider`) => {
        sliderLabel.text_=("Loops="+loopSlider.value)
      }
      case ValueChanged(`frameRateSlider`) => {
        frameRate = frameRateSlider.value
        frameRateLabel.text_=("Frame Rate="+frameRate)
      }
      case ButtonClicked(`cancelButton`) => {
        if (loopNotName) {
        	dialWindow.close
        }
        else {
          fileNameDialog.close
        }
      }
      case ButtonClicked(`proceedButton`) => {
        if (loopNotName) {
          val cmds = createLoop2(selFiles,pfix,loopSlider.value)
        
          //runCommands(cmds)
          infoArea.append("The looping procedure copied "+cmds+" files\n")
          loopDone = true
          loopNow.enabled_=(false)
          dialWindow.close
        }
        else {
          singleMovieName = nameField.text
          createSingleMovie(selFiles)
          fileNameDialog.close
        }
      }
      case ButtonClicked(`makeMovie`) => {
        if (loopDone || normalized) {
          val f0 = selFiles.first
          createMovie(f0.getParent,pfix+f0.getName)
        }
        else if (manyFiles) {
          processManyFiles(selFiles)
        }
        else {
          val f0 = selFiles.first
          createMovie(f0.getParent,f0.getName)
        }
        loopNow.enabled_=(true)
      }
      case ButtonClicked(`normalizeSeq`) => {
        normalizeSequence(pfix)
        makeMovie.enabled_=(true)
      }
      case ButtonClicked(`concatenateMovies`) => {
        loopNotName = false
        val fnPanel = new GridPanel(2,1) {
          val fnPanel2a = new GridPanel(1,2) {
            contents += nameField
            contents += new Label(".avi")
            
          }
          contents += fnPanel2a
          val fnPanel2b = new GridPanel(1,2) {
            contents += cancelButton
            contents += proceedButton
          }
          contents += fnPanel2b
          
        }
        fileNameDialog.contents_=(fnPanel)
        fileNameDialog.visible_=(true)
      }
      case ButtonClicked(`normalizeNow`) => {
        normalizeSequence(pfix)
        makeMovie.enabled_=(true)
      }
      case ButtonClicked(`clearAll`) => {
        selFiles = new LinkedList[File]()
        loopDone = false
        normalizeSeq.enabled_=(false)
        loopNow.enabled_=(false)
        makeMovie.enabled_=(false)
        openFiles.enabled_=(true)
        normalized = false
        normalizeNow.enabled_=(false)
        concatenateMovies.enabled_=(false)
        imagesLoaded = false
        infoArea.text_=("Cleared everything..\n please load new files\n")
        textArea.text_=("No files loaded\n")
      }
      case ButtonClicked(`reverseSequence`) => {
        reverseSeq("r")
      }
      case ButtonClicked(`closeApp`) => {
        quit
      }
      case ButtonClicked(`saveDir`) => {
        val chooser = new FileChooser
        chooser.fileSelectionMode_=(SelectionMode.DirectoriesOnly)
        //val filter = new FileFilter { def accept(file:File): Boolean = if (file.isDirectory) true else false; def getDescription : String = "myFilter" }
        //chooser.fileFilter_=(filter)
        val rval = chooser.showOpenDialog(contents.head)
        if (rval.toString.equals("Approve")) {
          saveDirectory = chooser.selectedFile.toString()+"/"
          saveLabel.text_=(saveDirectory)
        }
        
      }
      case ButtonClicked(`tmpDir`) => {
        val chooser = new FileChooser
        chooser.fileSelectionMode_=(SelectionMode.DirectoriesOnly)
        val rval = chooser.showOpenDialog(contents.head)
        if (rval.toString.equals("Approve")) {
          tmpDirectory = chooser.selectedFile.toString()+"/"
          tmpLabel.text_=(tmpDirectory)
        }
      }
      case ButtonClicked(`movDir`) => {
        val chooser = new FileChooser
        chooser.fileSelectionMode_=(SelectionMode.DirectoriesOnly)
        val rval = chooser.showOpenDialog(contents.head)
        if (rval.toString.equals("Approve")) {
          movieDirectory = chooser.selectedFile.toString+"/"
          movieDirLabel.text_=(movieDirectory)
        }
      }
      case ButtonClicked(`infoLauncher`) => {
        val infoWindow = new Dialog {

        }
        val aboutField = new TextArea("This is a simple GUI to be used for building movie files from image sequences\n" +
          		"You can send feature requests and bug reports to kuromaku@gmx.com\n"
            + "This only works properly in LINUX with ffmpeg installed")
        aboutField.editable_=(false)
        val infoCont = new FlowPanel(aboutField)
        infoWindow.contents_=(infoCont)
        infoWindow.pack
        infoWindow.centerOnScreen()
        infoWindow.visible_=(true)
      }
      case ButtonClicked(`saveConfig`) => {
        writeConfig
      }
    }
    //println(refactorString("/home/random/rec/images/noname_0138.jpg",30,4))
  }
  def getImages(dName:File) : Array[File] = {
    val files = dName.listFiles().filter(f => f.isFile && (
        f.getName.substring(f.getName.length-4).equals(".jpg") || f.getName.substring(f.getName.length-5).equals(".jpeg")))
    files
  }
  def fileNameChecker(fs:Seq[File]) : Boolean = {
    for (f <- fs) {
      val fname = f.getName
      if (regexpr.findFirstIn(fname) == None) {
        return false
      }
    }
    return true
  }
  def processAllFiles(fs:Seq[File]) : Unit = { //(Array[Int],Set[String])
    
    val numFiles = fs.length
    var fileInfo = new ListSet[String]()
    val endL = 9
    for (f <- fs) {
      val name = f.getName
      val l = name.length
      val sname = name.substring(0,l-endL)
      if (!fileInfo.contains(sname)) {
        fileInfo = fileInfo + sname
      }
    }
  }
  def processFiles2(fs:Seq[File]) : Boolean = fileNameChecker(fs)
  
  def processFiles(fs:Seq[File]) : ((Int,Int),LinkedList[Int]) = {
    if (!fileNameChecker(fs)) {
      properlyIndexedSequences = false
      return ((0,fs.length),new LinkedList[Int]())
    }
    else {
      properlyIndexedSequences = true
    }
    val numFiles = fs.length
    var indexes = new Array[Int](numFiles)
    textArea.text_=("")
    var idx = 0
    for (f <- fs) {
      val name = f.getName()
      val l = name.length
      textArea.append(name+"\n")
      indexes(idx) = name.substring(l-8,l-4).toInt
      idx += 1
    }
    indexes = indexes.sortWith((a,b) => a < b)
    var min = 10000
    var max = 0
    var prev = 0
    var breakPoints = new LinkedList[Int]()
    for (i <- indexes) {
      //println(i)
      if (i < min) {
        min = i
      }
      else if (i > max) {
        max = i
      }
      if (i > prev+1) {
        breakPoints :+ i
      }
    }
    ((min,max),breakPoints)
  }
  def processManyFiles(fs:Seq[File]) : Unit = {
    if (!fileNameChecker(fs)) {
      properlyIndexedSequences = false
      //val cmds = new Array[String](fs.length)
      var idx = 0
      var filescopied = 0
      for (f <- fs) {
        if (copyImage(f.getParent+"/"+f.getName,tmpDirectory+renamedFile(givenName+"_",idx,1))) {
          filescopied += 1
        }
        idx += 1
      }
      if (idx > filescopied) {
        infoArea.append("Only copied "+filescopied+" out of "+idx+" files\n")
      }
      //runCommands(cmds)
    }
    else {
      properlyIndexedSequences = true
      val names = countSeparate(fs)
    //val minMax = new Array[(Int,Int)](names.length)
    //val idxs = new Array[LinkedList[Int]](names.length)
      val nameSeqs = new Array[LinkedList[File]](names.length)
      for (i <- 0 until names.length) {
        nameSeqs(i) = new LinkedList[File]()
      }
      for (f <- fs) {
        val name = f.getName
        val l = name.length
        var found = false
        var idx = 0
        while (!found) {
          if (names(idx).equals(name.substring(0,l-8))) {
            found = true
          }
          else {
            idx += 1
          }
        }
        nameSeqs(idx) = nameSeqs(idx) :+ f
      }
      for (i <- 0 until names.length) {
        val (a,b) = processFiles(nameSeqs(i))
        //minMax(i) = a
        //idxs(i) = b
        if (a._1 == 1) {
          normalized = false
          val file = nameSeqs(i).head
          createMovie(file.getParent,file.getName)
        }
        else {
          normalizeSequence2(nameSeqs(i),"normalized",a._1)
          createMovie(movieDirectory,"normalized"+nameSeqs(i).head.getName)
        }
      }
    }

    //(minMax,idxs)
  }
    def createSingleMovie(fs:Seq[File]) : Unit = {
    val names = countSeparate(fs)
    val nameSeqs = new Array[LinkedList[File]](names.length)
    for (i <- 0 until names.length) {
      nameSeqs(i) = new LinkedList[File]()
    }
    for (f <- fs) {
      val name = f.getName
      val l = name.length
      var found = false
      var idx = 0
      while (!found) {
        if (names(idx).equals(name.substring(0,l-8))) {
          found = true
        }
        else {
          idx += 1
        }
      }
      nameSeqs(idx) = nameSeqs(idx) :+ f
    }
    var idx = 1
    val rname = tmpDirectory+singleMovieName
    for (i <- 0 until names.length) {
      val (a,b) = processFiles(nameSeqs(i))
      //minMax(i) = a
      //idxs(i) = b
      var nA = new Array[String](nameSeqs(i).size)
      var k = 0
      for (f <- nameSeqs(i)) {
        nA(k) = f.getPath
        k += 1
      }
      nA = nA.sortWith(String.lt)
      if (a._1 == 1 && b.size < 1) {
        for (k2 <- 0 until k) {
          //val aname = f.getName
          //val idx2 = aname.substring(l-8,l-4).toInt
          if (idx < 10) {
        	  copyImage(nA(k2),rname+"_000"+idx+".jpg")
          }
          else if (idx < 100) {
            copyImage(nA(k2),rname+"_00"+idx+".jpg")
          }
          else if (idx < 1000){
            copyImage(nA(k2),rname+"_0"+idx+".jpg")
          }
          else {
            copyImage(nA(k2),rname+"_"+idx+".jpg")
          }
          idx += 1
        }
      }
    }
    val runt = Runtime.getRuntime
    val cmd = "ffmpeg -f image2 -i "+rname+"_%04d.jpg -r "+frameRate+" -sameq "+movieDirectory+singleMovieName+".avi"
    runt.exec(cmd)
  }
  def createSingleMovie2(fs:Seq[File]) : Unit = {
    val names = countSeparate(fs)
    val nameSeqs = new Array[LinkedList[File]](names.length)
    for (i <- 0 until names.length) {
      nameSeqs(i) = new LinkedList[File]()
    }
    for (f <- fs) {
      val name = f.getName
      val l = name.length
      var found = false
      var idx = 0
      while (!found) {
        if (names(idx).equals(name.substring(0,l-8))) {
          found = true
        }
        else {
          idx += 1
        }
      }
      nameSeqs(idx) = nameSeqs(idx) :+ f
    }
    var idx = 1
    val rname = tmpDirectory+singleMovieName
    for (i <- 0 until names.length) {
      val (a,b) = processFiles(nameSeqs(i))
      if (a._1 == 1 && b.size < 1) {
        for (f <- nameSeqs(i)) {
          val aname = f.getName
          if (idx < 10) {
        	  copyImage(f.getParent+"/"+aname,rname+"_000"+idx+".jpg")
          }
          else if (idx < 100) {
            copyImage(f.getParent+"/"+aname,rname+"_00"+idx+".jpg")
          }
          else if (idx < 1000){
            copyImage(f.getParent+"/"+aname,rname+"_0"+idx+".jpg")
          }
          else {
            copyImage(f.getParent+"/"+aname,rname+"_"+idx+".jpg")
          }
          idx += 1
        }
      }
    }
    val runt = Runtime.getRuntime
    val cmd = "ffmpeg -f image2 -i "+rname+"_%04d.jpg -r "+frameRate+" -sameq "+movieDirectory+singleMovieName+".avi"
    runt.exec(cmd)
  }
  def countSeparate(fs:Seq[File]) : Array[String] = {
    var nameSet = new ListSet[String]()
    for (f <- fs) {
      val name = f.getName
      val l = name.length
      val n = name.substring(0,l-8)
      nameSet = nameSet + n
    }
    nameSet.toArray
  }
  def createLoop(fs:Seq[File],prefix:String) : Int = {
    val l = fs.length
    var idx = 0
    for (f <- fs) {
      val name = f.getName
      val l = name.length
      val num = name.substring(l-8,l-4).toInt
      val dName = f.getParent
      if (num != max1) {
        if (copyImage(dName+"/"+name,tmpDirectory+prefix+renamedFile(name.substring(0,l-8),(max1-num),max1))) {
          idx += 1
        }
        if (copyImage(dName+"/"+name,tmpDirectory+prefix+name)) {
          idx += 1
        }
        
      }
      else {
        if (copyImage(dName+"/"+name,tmpDirectory+prefix+name)) {
          idx += 1
        }
      }
      
    }
    idx
  }
  def createLoop2(fs:Seq[File],prefix:String,loops:Int) : Int = {
    val l = fs.length
    val commands = new Array[String](loops*l-loops+1)
    
    val fA:Array[File] = fs.toArray
    var sA = new Array[String](l)
    for (i <- 0 until l) {
      sA(i) = fA(i).getParent+"/"+fA(i).getName
    }
    sA = sA.sortWith(String.lt)
    val namLength = fA(0).getName.length
    val dirLength = fA(0).getParent.length
    var copied = 0
    
    for (i <- 0 until l) {
      if (copyImage(sA(i),tmpDirectory+prefix+sA(i).substring(dirLength+1))) { copied += 1 }
    }
    var idx = l
    var up = false
    val name = fA(0).getName.substring(0,fA(0).getName.length-8)
    for (j <- 2 until (loops+1)) {
      if (!up) {
        for (k <- 1 until l) {
          if (copyImage(sA(l-k-1),tmpDirectory+prefix+renamedFile(name,idx,1))) { copied += 1 }
          idx += 1
        }
      }
      else {
        for (k <- 1 until l) {
          if (copyImage(sA(k),tmpDirectory+prefix+renamedFile(name,idx,1))) { copied += 1 }
          idx += 1
        }
      }
      up = !up
    }
    copied
  }
  def normalizeSequence(preFix:String) : Unit = {
    if (min1 > 1) {
      
      infoArea.append("Normalizing image sequence...")
      for (f <- selFiles) {
        val name = f.getName
        val dPath = f.getParent
        val l = name.length
        val idx = name.substring(l-8,l-4).toInt
        val name2 = refactorString(name,min1-1,4)
        copyImage(dPath+"/"+name,saveDirectory+preFix+name2)
      }
      /*
      val runTime = Runtime.getRuntime
      for (c <- cmds) {
        runTime.exec(c)
      }
      */
      normalized = true
      
    }
    
  }
  def normalizeSequence2(files:Seq[File],preFix:String,min0:Int) : Unit = {
    if (min0 > 1) {
      infoArea.append("Normalizing filenames..."+"\n")
      for (f <- files) {
        val name = f.getName
        val dPath = f.getParent
        val l = name.length
        val idx = name.substring(l-8,l-4).toInt
        val name2 = refactorString(name,min0-1,4)
        copyImage(dPath+"/"+name,saveDirectory+preFix+name2)
      }
      /*
      val runTime = Runtime.getRuntime
      for (c <- cmds) {
        runTime.exec(c)
      }
      */
      
      normalized = true
      
    }
    
  }
  def readConfig(fName:String) : Unit = {
    val configFile = new File(fName)
    if (configFile.exists()) {
      val config = XML.loadFile(configFile)
      tmpDirectory = (config \\ "TmpDir").text
      saveDirectory = (config \\ "SaveDir").text
      movieDirectory = (config \\ "VideoDir").text
      frameRate = (config \\ "FrameRate").text.toInt
    }
    else {
      infoArea.append("Could not read a config file...\n")
      //println(configFile.getPath)
    }
  }
  def writeConfig : Unit = {
    val config = <SaveDir>{saveDirectory}</SaveDir><VideoDir>{movieDirectory}</VideoDir><TmpDir>{tmpDirectory}</TmpDir><FrameRate>{frameRate}</FrameRate>;
      
    val fw = new FileWriter("videoBuilderConfig.xml")
    //val el = <VideoBuilderConfig>
    //scala.xml.XML.write(fw,el,"UTF-8",true,null)
    val config4 = <videoBuilderConfig>{config}</videoBuilderConfig>
    scala.xml.XML.write(fw,config4.head,"UTF-8",true,null)
    config4.tail.foreach(e => scala.xml.XML.write(fw,e,"UTF-8",false,null))
    fw.close
    infoArea.append("Saved current configuration.\n")
  }
  def reverseSeq(prefix:String) : Unit = {
    infoArea.append("Creating a reversed sequence...\n")
    for (f <- selFiles) {
      val name = f.getName
      val dPath = f.getParent
      val l = name.length
      val idx = name.substring(l-8,l-4).toInt
      val name2 = prefix+renamedFile(name.substring(0,l-8),-idx,max1+1)
      copyImage(dPath+"/"+name,tmpDirectory+name2)
    }
    
    reversed = true
  }
  def renamedFile(n:String,add:Int,idx:Int) : String = {
    val nidx = idx+add
    
    if (nidx > 999) {
      n+nidx+".jpg"
    }
    else if (nidx > 99) {
      n+"0"+nidx+".jpg"
    }
    else if (nidx > 9) {
      n+"00"+nidx+".jpg"
    }
    else {
      n+"000"+nidx+".jpg"
    }
  }
  def refactorString(s:String,reduction:Int,endlength:Int) : String = {
    var idx = s.length()-endlength
    val send = s.substring(idx,s.length())
    var b = false
    var state = 1
    var sum:Int = 0
    while (!b && idx > 0) {
      val c = s.charAt(idx)
      if (c.isDigit) {
        sum += state*(c.toInt - '0'.toInt)
        state *= 10
      }
      else if (state > 1) {
        b = true
      }
      idx -= 1
    }
    sum -= reduction
    if (state > 0) {
      val s2 = s.substring(0,idx+2)
      if (sum > 999) {
        return s2+sum+send
      }
      else if (sum > 99) {
        return s2+"0"+sum+send
      }
      else if (sum > 9) {
        return s2+"00"+sum+send
      }
      else {
        return s2+"000"+sum+send
      }
    }
    else {
      infoArea.append("An error occurred while refactoring a filename: "+s)
      "fail"
    }
    
  }
  def runCommands(cmd:Array[String]) : Unit = {
    val runt = Runtime.getRuntime
    for (c <- cmd) {
      runt.exec(c)
      infoArea.append("Ran command: "+c+"\n")
    }
  }
  def createMovie(dNam:String,imgs:String) : Unit = {
    if (loopDone || normalized || reversed) {
      val cmd = "ffmpeg -f image2 -i "+tmpDirectory+imgs.substring(0,imgs.length-8)+"%04d.jpg -r "+frameRate+" -sameq "+movieDirectory+imgs.substring(0,imgs.length-9)+".avi"
      infoArea.append("Created a movie with the command: "+cmd+"\n")
      val runt = Runtime.getRuntime
      runt.exec(cmd)
      
    }
    else if (!properlyIndexedSequences) {
      val cmd = "ffmpeg -f image2 -i "+tmpDirectory+"notproper_%04d.jpg -r "+frameRate+" -sameq "+movieDirectory+"notproper.avi"
      infoArea.append("Created a movie with the command: "+cmd+"\n")
      val runt = Runtime.getRuntime
      runt.exec(cmd)
    }
    else {
      val cmd = "ffmpeg -f image2 -i "+dNam+"/"+imgs.substring(0,imgs.length-8)+"%04d.jpg -r "+frameRate+" -sameq "+movieDirectory+imgs.substring(0,imgs.length-9)+".avi"
      infoArea.append("Created a movie with the command: "+cmd+"\n")
      val runt = Runtime.getRuntime
      runt.exec(cmd)
    }
    //
    //
  }
}
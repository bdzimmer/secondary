// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Page templates. Originally part of the old "Bootstrap" class.
// Ben Zimmer

// 2015-08-10: Created in refactor.

package bdzimmer.secondary.export

import bdzimmer.secondary.export.Tags._


/**
 *
 * Static class for generating Bootstrap HTML.
 *
 */

object PageTemplates {

  val COLUMN_12 = 12

  val pageTemplate= """
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <meta name="description" content="">
    <meta name="author" content="">


    <title>%s</title>

    <!-- Bootstrap core CSS -->
    <link href="bootstrap/css/bootstrap.min.css" rel="stylesheet">

    <!-- Custom styles for this template -->
    <!-- <link href="starter-template.css" rel="stylesheet"> -->

    <!-- Just for debugging purposes. Don't actually copy these 2 lines! -->
    <!--[if lt IE 9]><script src="../../assets/js/ie8-responsive-file-warning.js"></script><![endif]-->
    <script src="../../assets/js/ie-emulation-modes-warning.js"></script>

    <!-- HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries -->
    <!--[if lt IE 9]>
      <script src="https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js"></script>
      <script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
    <![endif]-->

    <style>
      %s
    </style>

  </head>

  <body>

    %s

    <!-- Bootstrap core JavaScript
    ================================================== -->
    <!-- Placed at the end of the document so the pages load faster -->
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"></script>
    <script src="bootstrap/js/bootstrap.min.js"></script>
    <!-- IE10 viewport hack for Surface/desktop Windows 8 bug -->
    <script src="../../assets/js/ie10-viewport-bug-workaround.js"></script>
  </body>
</html>
  """


  def createPage(outputFile: String, title: String, styles: String, body: String): Unit = {

    val pageText = pageTemplate.format(title, styles, body)

    val fileWriter = new java.io.FileWriter(outputFile, false)
    fileWriter.write(pageText)
    fileWriter.close

  }



  def createArticlePage(outputFile: String,
                        title: String, description: String, toolbar: Option[String],
                        body: String, license: String): Unit = {

    createPage(
      outputFile,
      title,

      "",

      container(
        column(COLUMN_12,
          ("<h1>%s</h1><h3>%s</h3>".format(title, description)) +
          toolbar.getOrElse("") +
          hr
        ) +
        body
      ) +
      container(hr + centered(license))
    )

  }




}

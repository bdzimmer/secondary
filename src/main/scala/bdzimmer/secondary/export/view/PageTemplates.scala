// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Page templates. Originally part of the old "Bootstrap" class.
// Ben Zimmer

// 2015-08-10: Created in refactor.
// 2015-09-02: All pages use jumbotron.
// 2015-09-06: Google Fonts and link to custom stylesheet in main page template.

package bdzimmer.secondary.export.view

import bdzimmer.secondary.export.view.Tags._

// Static class for generating Bootstrap HTML.
object PageTemplates {

  val Column12 = 12

  val NavbarSeparator = nbsp + nbsp + "&middot;" + nbsp + nbsp

  def createPage(outputFile: String, title: String, styles: String, body: String): Unit = {

    val pageText = s"""
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <meta name="description" content="">
    <meta name="author" content="">

    <title>${title}</title>

    <!-- Fonts -->
    <link href='https://fonts.googleapis.com/css?family=${Styles.FontFace.replace(' ', '+')}:400,400italic,700,700italic' rel='stylesheet' type='text/css'>
    <!-- Bootstrap core CSS -->
    <link href="styles/bootstrap/css/bootstrap.min.css" rel="stylesheet">
    <!-- Custom styles for Secondary -->
    <link href="styles/secondary.css" rel="stylesheet">

    <!-- HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries -->
    <!--[if lt IE 9]>
      <script src="https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js"></script>
      <script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
    <![endif]-->

    <!-- Bootstrap core JavaScript -->
    <!-- (Originally placed at the end of the document so the pages load faster) -->
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"></script>
    <script src="styles/bootstrap/js/bootstrap.min.js"></script>

    <!-- Additional styles -->
    <style>
      ${styles}
    </style>
  </head>

  <body>
    ${body}
  </body>
</html>
  """

    val fileWriter = new java.io.FileWriter(outputFile, false)
    fileWriter.write(pageText)
    fileWriter.close()
  }


  def createArticlePage(
      outputFile: String,
      title: String, description: String, navbar: Option[String],
      body: String, license: String): Unit = {

    createPage(
      outputFile,
      title,

      "",

      jumboTron(
          container(
              column(Column12,
                  "<h1>%s</h1><h3>%s</h3>".format(
                      Markdown.processLine(title),
                      Markdown.processLine(description))))) +

      container(
        column(Column12, navbar.getOrElse("")) +
        body +
        column(Column12, hr + centered(license))
      )
    )

  }

}

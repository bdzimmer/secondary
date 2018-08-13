// Copyright (c) 2018 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.view


object Bootstrap {
  
  val Column3 = 3
  val Column4 = 4
  val Column6 = 6
  val Column8 = 8
  val Column9 = 9
  val Column12 = 12
  
  
  def container(body: String): String = {
s"""
<div class="container">
  ${body}
</div>
"""
  }
  
  
  def row(body: String): String = {
s"""
<div class="row">
  ${body}
</div>
"""
  }


  def jumbotron(body: String): String = {
s"""
<div class="jumbotron">
  ${body}
</div>
"""
  }


  def column(size: Int, body: String): String = {
s"""
<div class="col-md-${size}">
  ${body}
</div>"""
  }

}

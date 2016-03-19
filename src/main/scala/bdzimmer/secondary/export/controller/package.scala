// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export

import com.google.api.client.util.DateTime


package object controller {

  type FileMap = scala.collection.immutable.Map[String, (String, DateTime)]
  type FileOutputsMap = scala.collection.immutable.Map[String, List[String]]
  type ItemMap = scala.collection.immutable.Map[String, (String, Int)]

}

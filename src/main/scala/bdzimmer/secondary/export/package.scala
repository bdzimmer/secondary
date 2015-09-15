// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary

import com.google.api.client.util.DateTime


package object export {

  type FileModifiedMap = scala.collection.immutable.Map[String, (String, DateTime)]
  type FileOutputsMap = scala.collection.immutable.Map[String, List[String]]

}

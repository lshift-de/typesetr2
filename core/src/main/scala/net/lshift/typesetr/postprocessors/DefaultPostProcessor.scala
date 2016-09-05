package net.lshift.typesetr
package postprocessors

import parsers.NodeConfigs

object DefaultPostProcessor {

  private class OptimizerAndMetaExtractor[T](config: NodeConfigs.WithNode[T])
    extends PostProcessor[T]
    with Optimizer[T]
    with PostProcessorUtils[T]
    with OptimizerCoalesceParentChild[T]
    with OptimizerCoalesceBlocks[T]
    with OptimizerCoalesceSiblings[T]
    with OptimzerCoalesceHeadings[T]
    with MetaInferencer[T] {

    protected def nodeConfig: NodeConfigs.WithNode[T] = config

  }

  def fromConfig[T](config: NodeConfigs.WithNode[T]): PostProcessor[T] =
    new OptimizerAndMetaExtractor(config)

}

open Rejest;

open TestReconciler;

type tree('a) =
  | TreeNode('a, list(tree('a)))
  | TreeLeaf('a);

let validateStructure = (rootNode: node, structure: tree(primitives)) => {
  let rec f = (inputNode: node, st: tree(primitives), level) =>
    switch (st) {
    | TreeNode(p, c) =>
      expect(inputNode.nodeType).toEqual(p);
      expect(List.length(inputNode.children^)).toEqual(List.length(c));

      List.iter2((a, b) => f(a, b, level + 1), inputNode.children^, c);
    | TreeLeaf(p) => expect(inputNode.nodeType).toEqual(p)
    };

  f(rootNode, structure, 0);
};

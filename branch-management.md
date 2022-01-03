# Branch management

## How to do GitHub Flow

1. command: git checkout -b _branch_name_
2. command: git push origin _branch_name_
3. work: update version number
4. command: git push --set-upstream origin _branch_name_
5. work: development
6. command: git checkout master
7. command: git merge _branch_name_
8. command: git push
9. command: git tag _version_
10. command: git push origin _version_


## Branches

* 20220103-pbr2
    - version 2.7.0.2
    - PBRの修正
        - MaterialとSurfaceの分離やり直し
        - 開発中断分の取り入れ
        - 幾何減衰対応（n'や反射屈折方向が面の裏側になった場合の処置）

---

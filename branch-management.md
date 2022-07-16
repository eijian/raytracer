# Branch management

## How to do GitHub Flow

1.  command: git checkout -b _branch_name_
2.  command: git push origin _branch_name_
3.  work: update version number
4.  command: git push --set-upstream origin _branch_name_
5.  work: development
6.  work: push all changes
7.  command: git checkout master
8.  command: git merge _branch_name_
9.  command: git push
10. command: git tag _version_
11. command: git push origin _version_


## Branches

* 20220715-miniupdate
    - version 2.7.4.0
    - 小改善
        - 各光源毎にuse classicパラメータを持つ
        - 光源仕様をオブジェクトの一要素にし、光源リストはオブジェクトから生成
        - kd-treeを分割し、フォトン数が多い場合の処理時間を短縮
        - use classic 時に各光源からの輝度が無い場合はNothing
        - use classic 時の光源方向ベクトルは、現状の方式と光源の法線方向へのベク
          トルから求める方法の２つを実施、比率は光源の指向性による。

* 20220704-bugfix
    - version 2.7.3.1
    - バグフィックス
        - ○ use classic にすると異常に遅い
        
* 20220305-mapping
    - version 2.7.3.0
    - マッピングサポート
        - ○ 表面点(SurfacePoint)の導入
        - ○ 交点計算で(U,V)を返す
        - × Material, Surfaceをリスト化する
        - ○ マッピング用変換関数(Mapper)の導入
        - 入力は表面点+(U,V)、出力はMaterial, Surface、表面点'
        - ○ 関数による2Dテクスチャ（チェック、ストライプ、など）
        - ×ノイズテクスチャ
        - ×displacement mappingの考慮

* 20220118-skylight
    - version 2.7.2.1
    - スカイライトの追加
        - ○ フォトン放射方向の反転
        + ○ 直接光を計算で求める(=useClassic)
        - NG 光源の属性に直接光を計算で求めることを強制するしくみ。

* 20220111-mesh_support
    - version 2.7.2.0
    - メッシュ（三角パッチ集合）のサポート
        - ○ メッシュ型の新設
        - ○ 交点計算時にオブジェクト毎にリストを返すのではなくMaybe値を返す
        - NG 法線ベクトル補間（スムースシェーディング）

* 20220110-code_cleaning
    - version 2.7.1.1
    - コードの整理整頓
        - ○ warningをできるだけ出ないように
        - ○ 古典レイトレーシングの削除
        - ○ 短すぎる変数名の改善
        - ○ 過去の試行部分（不要なコメントアウトしたコード）の除去

* 20220108-newlight
    - version 2.7.1.0
    - 光源の再定義
        - ○ 色温度による光源色指定
        - ○ 光源と物体との統合
        - ○ 光線の指向性の導入

* 20220103-pbr2
    - version 2.7.0.2
    - PBRの修正
        - ○ MaterialとSurfaceの分離やり直し
        - ○ 開発中断分の取り入れ
        - ○ 幾何減衰対応（n'や反射屈折方向が面の裏側になった場合の処置）
        - NG PhysicsモジュールをOpticsモジュールへ統合
        - NG BSDFの改善
        - △ 反射率(鏡面アルベド)は屈折率から求める（金属=複素数屈折率は後）


## Backlog

- 再帰追跡時に反射率を累積して一定以下の場合はそれ以上追跡しない
- カメラの整理
- パーサーの作り直し
- pmとrtの統合
- Rubyスクリプトからの脱却
- ポリゴンの法線補間（スムースシェーディング）
- 光源の属性に直接光を計算で求めることを強制するしくみ
- SkyLightのバグ取り（マダラ、影なし）
- 簡易モデリング言語(Ruby)
- 反射率(鏡面アルベド)は屈折率から求める（金属=複素数屈折率は後）
- ノイズテクスチャ
- displacement mappingの考慮
- Material, Surfaceのリスト化

---

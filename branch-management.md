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

* 20240905-miniupdate
    - version-2.8.3.0
    - 小改善
        - △ 輝度推定半径の指定：初期値ではなく最終値を指定する → 画質の制御が難しいので保留
        - ⚪︎ GHC9.2で導入のRecord Dot Syntaxに対応する


* 20240105-camera2
    - version-2.8.2.0
    - カメラサポートその2
        - ⚪︎露出の正しい定義
        - ⚪︎露出補正のサポート
        - ⚪︎ホワイトバランスのサポート
        - ⚪︎正しいボケ（円形）のサポート
        - ⚪︎OpenEXRに追加ヘッダーを入れる（F値、フォーカス距離、シャッター時間など）

* 20221021-modeler
    - version-2.8.1.0
    - - 簡易モデリング言語(Ruby)
        - ライブラリ作成
        - サンプルシーン作成

* 20221010-camera
    - version-2.8.0.0
    - カメラのサポート
        - ○ F値、焦点距離、フォーカス距離の整理
        - ○ シャッタースピードとISO感度で明るさを表す

* 20221001-merge_pmrt
    - version-2.7.8.0
    - pmとrtを統合する
        - ○ ppmコマンドを作る
        - ○ 複数フォトンマップから単一フォトンマップに戻す

* 20220930-miniupdate
    - version-2.7.7.0
    - 屈折のバグフィックスと小改善
        - ○ 透明体内部からの屈折処理のバグ修正
        - ○ Meshの交点判定バグ
        - ○ 反射屈折の打ち切り条件に反射率の減衰を加味
        - ○ Meshの交点判定時、すぐにPolygonを生成せずPatchを返す

* 20220915-improvepbr
    - version-2.7.6.0
    - PBR処理の改善
        - ○ 無駄な探索の排除
        - ○ 金属か誘電体かで分離

* 20220911-bugfix
    - version-2.7.5.1
    - 透過率のバグフィックス
        - レンダリング時に透明物体から届く光が透過率を反映していない件の調査対応

* 20220821-newparser
    - version 2.7.5.0
    - パーサの再作成
        - ○ 物理ベースレンダリング、オブジェクトや光源の見直しに応じたパーサの見直し
        - ○ UVマッピング座標のサポート
        - ○ 反射率(鏡面アルベド)は屈折率から求める（金属=複素数屈折率は後）
        - ○ Sky dome のバグ取り（マダラ、影なし）
        - ○ 光源の属性に直接光を計算で求めることを強制するしくみ
        - △ Material, Surfaceのリスト化 -> パーサ再構築により実現


* 20220715-miniupdate
    - version 2.7.4.0
    - 小改善
        - ○ 各光源毎にuse classicパラメータを持つ
        - ○ 光源仕様をオブジェクトの一要素にし、光源リストはオブジェクトから生成
        - ○ kd-treeを分割し、フォトン数が多い場合の処理時間を短縮
        - ○ use classic 時に各光源からの輝度が無い場合はNothing

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
        - × 入力は表面点+(U,V)、出力はMaterial, Surface、表面点'
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

- カメラの整理
- OpenEXRに追加ヘッダーを入れる（F値、フォーカス距離、シャッター時間など）
- Rubyスクリプトからの脱却
- ポリゴンの法線補間（スムースシェーディング）
- ノイズテクスチャ
- displacement mappingの考慮
- 入力は表面点+(U,V)、出力はMaterial, Surface、表面点'
- use classic 時の光源方向ベクトルは、現状の方式と光源の法線方向へのベク
  トルから求める方法の２つを実施、比率は光源の指向性による。
- 拡散反射のOren-Nayerモデルの考慮
- フォトンの色をRGBではなく紫外線〜赤外線まで対応する

---

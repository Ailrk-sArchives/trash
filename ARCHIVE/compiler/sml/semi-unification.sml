(*
 * https://en.wikipedia.org/wiki/RE_(complexity)
 *
 * Recursive enumerable is the class of decision problem which true or false
 * answer can be verified by turing machine in a finite amount of time.
 *
 * charactristics:
 *  1. if answer is yes, it halts in finite time.
 *  2. if is no, it's not necessary to halt.
 *)

(*
 * A semi-algorithm is like an algorithm but does not guarantee to halt in all
 * inputs. It corresponds to recursive enumerable problem.
 *)

(*
 * Type inference of Polymorphic recursion requires semi-unification, which
 * itself is semi algorithm, thus
 * RE is related to polymorphic recursion which is proven has undecidable
 * type inference.
 *
 * [Semi unification problem is undecidable]
 * https://pdf.sciencedirectassets.com/272575/1-s2.0-S0890540100X01307/1-s2.0-S0890540183710035/main.pdf?X-Amz-Security-Token=IQoJb3JpZ2luX2VjEBcaCXVzLWVhc3QtMSJGMEQCIDpGfsc9NXYZR8gsKyquylJFF8pJvSk1LsqxPKTf0XPxAiB%2BmGgxscXEGgkeuStdsPua%2FWkGGpIlSqpzEy7iaQuFayqDBAiA%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F8BEAQaDDA1OTAwMzU0Njg2NSIMQOKmjUTtqX6wNfm2KtcD06W6Mk%2FGLYuGCoOWFxmvrDyH7RM3Dat%2BYYxiXJqnsWpUboAkdRNjN654FS5jUudcEKHpKeOou3YROWhL57VPVXAeKyE4E8h9eLp%2F2pmlFb1X5BjAzf2iJbsIXmkWuLDmfgsfmnI8isbG1SxusN9YhVsdib3Vg8pI0ZR36TZxLAGGiq8xxwfH1Dw0mdsqB8LLf%2BzshZ0BwlVNcNrew41IJTlL23d%2BqKJqnSmVHoxopd8S3oTYGQSLSDxSe1FxxqC%2F7X39078ZsAG8qO0ylD%2B1skZ9ML%2FLFWf1PI8X54SzqQY80tnI6nEpyyhFgRq0AL5V2vJBOjaGGRo0CzAffpLFZ%2FfnNnKQiTwa1i0xVLVMLAk6DWtQo%2BPdy8rBonTwukxVnh1k7rZgz19CFX4m33ZSZbJ%2FBoL0LdL3yQyedaF41GpKpMK%2B9NP%2BtjnO%2B%2FP8xPeTOLi8Mc6gzYMAUaVAx4F5pOGBoZs49cKGXArmjg9mi7rp21oR0EjeGB0n5aBgq8Kg%2Fd6M3ux0Ctu7U61jmsYQR2ngh4U8Ba5gzUZFiAU4QsvYE5dSnyfnhBb1V2DRf7%2FDArjvomzFc0a3Mf%2Fmw6hjOXlBAXC0cdy4acWmnSq8XxX6dSA5thjpMJ%2Fmw4oGOqYBotqbwHAGe1GkWPM8MR7HGfismGu3BHbvSGpcUCJjpExn6yuvRd709SPAlmPxzuvzRY5%2FGCkHjjNJ7jbNDahDrjL5%2BzCaAzzysUmiq491Bh2nDK6J8ZT%2FBcI1fQiHx16yIYg54EGpVqvenv%2FbZnU9KsscynwRxgLuv9QtjVqk8i2xF6he4pX%2BiBDZTTaLZvT5KCkllZlJBOnA254wzYiXUNzcmhWh9g%3D%3D&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20210927T001155Z&X-Amz-SignedHeaders=host&X-Amz-Expires=300&X-Amz-Credential=ASIAQ3PHCVTYRSOHJNW2%2F20210927%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=37e6593032e33451ec91136253909995e07aca396efb93e6276a159f5dfc5c0e&hash=9520f40862340c1ac332e1835f691270991992b9c240cdbf953e60cdded9c422&host=68042c943591013ac2b2430a89b270f6af2c76d8dfd086a07176afe7c76c2c61&pii=S0890540183710035&tid=spdf-c224c7b7-0829-4259-829c-1943ebbc5b74&sid=3ff4997c6cffe9446158b5858e9d283933b1gxrqa&type=client
 *
 * [Fast algorithms for unifiorm semi-unification]
 * https://core.ac.uk/download/pdf/82256808.pdf
 * *)
